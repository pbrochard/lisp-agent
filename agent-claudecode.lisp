;;;; agent-claudecode.lisp — drive the Claude Code CLI as the model backend
;;;;
;;;; Where agent-claude.lisp pays per token straight to the Anthropic API,
;;;; this one shells out to the `claude` CLI already sitting on the box and
;;;; rides whatever auth it's logged in with (subscription, not API key).
;;;; No HTTP, no hand-rolled tool loop: the CLI has its own tools (Bash,
;;;; Edit, Read, ...) and its own agent loop. We just feed it a prompt and
;;;; print what comes back.
;;;;
;;;; Usage:
;;;;   sbcl --load agent-claudecode.lisp --eval '(agent-claudecode:run "...")'
;;;;
;;;; Verbose: (cc:verbose) toggles a live trace of the work itself -- every
;;;; tool the CLI runs, with its arguments, and a preview of what came back --
;;;; printed in grey alongside the usual thinking/answer stream.
;;;;
;;;; Tool history: every tool the CLI runs -- with its arguments and a
;;;; preview of its result -- is written to /agent/data/claudecode-tools.md
;;;; as the turn happens, whether or not the verbose trace is on. The file is
;;;; emptied at the start of each (run ...), so it always describes the
;;;; current turn.
;;;;
;;;; Memory: Claude Code keeps its own session transcript on disk. We only
;;;; remember the session id between runs so --resume picks the same
;;;; conversation back up; a copy of the exchange is also appended to
;;;; memory-claudecode.json for consistency with the other agents.
;;;;   (agent:run "My name is Jamie.")
;;;;   ...later, in a fresh process...
;;;;   (agent:run "What is my name?")   ; => it remembers
;;;;   (agent:forget)                   ; wipe the slate

(defpackage :agent-claudecode
  (:use :cl :utils :common :cl-ansi-text)
  (:export #:run #:use #:forget #:set-model #:list-models #:lm #:*models*
           #:set-effort #:list-efforts #:le #:*efforts* #:usage
           #:set-timezone #:*timezone*
           #:verbose #:set-verbose #:*verbose*)
  (:nicknames :cc :claudecode :ccode))

(in-package :agent-claudecode)

(defparameter *claude-bin* "claude")
(defparameter *model* "opus")
(defparameter *permission-mode* "bypassPermissions")

;;; Unlike agent-gemini/agent-claude, the CLI has no models endpoint to query
;;; (it rides subscription auth, not an API key), so this is just the fixed
;;; list of aliases --model accepts.
(defparameter *models* (vector "sonnet" "opus" "fable" "haiku"))

(defparameter *effort* nil)

;;; stdout (main thread, streamed deltas) and stderr (its own thread, see
;;; DRAIN-STDERR) both write to the same terminal. Without a shared lock
;;; their writes can interleave mid-line whenever both fire around the same
;;; time, which looks exactly like a long line getting garbled/truncated.
(defparameter *output-lock* (sb-thread:make-mutex :name "claude-output"))

;;; The rate_limit_info from the most recent real (non-local-command) call,
;;; cached so USAGE can show an exact reset countdown even though /usage
;;; itself is answered locally by the CLI and carries no rate-limit payload.
(defparameter *last-rate-limit* nil)

;;; --effort accepts a fixed set of levels; nil means "don't pass the flag"
;;; and let the CLI use its own default.
(defparameter *efforts* (vector "low" "medium" "high" "xhigh" "max"))

;;; When on, every tool the CLI runs is echoed as it happens -- which tool,
;;; with which arguments, and a preview of what it answered -- instead of the
;;; turn showing only the model's own prose. Off by default: on a long turn
;;; that is a lot of output, and it is meant for watching the work happen,
;;; not for normal reading. See SET-VERBOSE.
(defparameter *verbose* nil)

;;; Timezone the rate-limit reset times are rendered in. An IANA name, since
;;; a fixed UTC offset would be wrong half the year anywhere that keeps DST.
;;; Resolved against local-time's bundled zoneinfo, so no external process and
;;; no dependency on the host having tzdata installed.
(defparameter *timezone* "Europe/Paris")

;;; local-time loads its zone repository lazily, and until it has,
;;; FIND-TIMEZONE-BY-LOCATION-NAME just answers NIL for every name rather than
;;; complaining. Read it once, on first use: it costs ~50ms.
(defparameter *timezone-repository-loaded* nil)

(defun find-timezone (name)
  "The local-time timezone object for IANA NAME, or NIL if there is no such zone."
  (unless *timezone-repository-loaded*
    (local-time:reread-timezone-repository)
    (setf *timezone-repository-loaded* t))
  (ignore-errors (local-time:find-timezone-by-location-name name)))

(defconstant MEMORY-FILE "/agent/data/memory-claudecode.json")
(defconstant SESSION-FILE "/agent/data/session-claudecode.txt")
(defconstant TOOLS-FILE "/agent/data/claudecode-tools.md")

;;; --- session id persistence ---------------------------------------------
;;; The CLI already remembers the full transcript per session; all we need
;;; to survive a fresh process is which session id to --resume.

(defun read-session-id ()
  (when (probe-file SESSION-FILE)
    (with-open-file (in SESSION-FILE)
      (read-line in nil nil))))

(defun write-session-id (id)
  (with-open-file (out SESSION-FILE :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (write-line id out)))

;;; --- talking to the model -------------------------------------------------
;;; No HTTP here: the "call" is a subprocess in print mode, JSON in, JSON out.
;;; We ask for stream-json (one JSON object per line) instead of a single
;;; json blob because that is the only format on which the CLI reports
;;; rate_limit_event lines alongside the final result line. --include-partial-
;;; messages additionally breaks each message into content_block_delta events
;;; (thinking_delta, text_delta, ...) so we can render them as they arrive
;;; instead of only seeing the finished message.

(defconstant +UNIX-EPOCH-UNIVERSAL-TIME+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun unsafe-terminal-char-p (ch)
  "True for a character that could rewrite/erase already-printed terminal
output or visually spoof it: C0 controls (backspace, carriage return, ESC,
...), DEL, C1 controls (0x80-0x9F, the 8-bit equivalents of ESC-introduced
CSI/OSC sequences some terminals honor), and Unicode bidi-override /
directional-isolate formatting characters (U+202A-U+202E, U+2066-U+2069)
that terminals honoring bidi can use to visually reorder displayed text.
Newlines and tabs are left alone."
  (let ((code (char-code ch)))
    (or (= code 127)                                 ; DEL
        (<= #x80 code #x9F)                          ; C1 controls
        (<= #x202A code #x202E)                      ; bidi embeddings/overrides
        (<= #x2066 code #x2069)                      ; bidi isolates
        (and (< code 32) (not (member code '(9 10)))))))

(defun sgr-sequence-end (string start)
  "If a well-formed SGR (color/attribute) escape sequence ESC[<params>m
starts at STRING's index START (which must be the ESC byte itself), return
the index just past its final #\\m; otherwise NIL. SGR is the one escape
shape that only changes how subsequent text is rendered — it can't move
the cursor, erase anything, or touch the screen — so it's safe to let
through even though model output is otherwise untrusted."
  (when (and (< (1+ start) (length string))
             (char= (char string (1+ start)) #\[))
    (loop for i from (+ start 2) below (length string)
          for ch = (char string i)
          do (cond
               ((or (digit-char-p ch) (char= ch #\;)))
               ((char= ch #\m) (return (1+ i)))
               (t (return nil)))
          finally (return nil))))

(defun strip-terminal-control-chars (string)
  "Strip characters that could rewrite, erase, or visually spoof
already-printed terminal output (see UNSAFE-TERMINAL-CHAR-P), while letting
well-formed SGR color/attribute sequences through unharmed (see
SGR-SEQUENCE-END) so legitimate coloring still works. Model output is
otherwise untrusted and must not be allowed to manipulate the terminal
it's printed to. Returns STRING unchanged (no copy) when nothing needs
stripping, since that is the overwhelmingly common case on the streaming
hot path."
  (if (find-if #'unsafe-terminal-char-p string)
      (with-output-to-string (out)
        (loop with len = (length string)
              with i = 0
              while (< i len)
              do (let ((sgr-end (and (char= (char string i) #\Escape)
                                      (sgr-sequence-end string i))))
                   (cond
                     (sgr-end
                      (write-string string out :start i :end sgr-end)
                      (setf i sgr-end))
                     ((unsafe-terminal-char-p (char string i))
                      (incf i))
                     (t
                      (write-char (char string i) out)
                      (incf i))))))
      string))

;;; --- the verbose tool trace ----------------------------------------------
;;; The CLI announces each tool it runs as a tool_use content block, and what
;;; the tool answered as a matching tool_result block in the following
;;; message. Neither is shown at all unless *VERBOSE* is on, in which case
;;; each becomes one grey line in the stream, in between the model's own
;;; paragraphs.

(defparameter *verbose-value-width* 160
  "How much of a single tool argument is shown. A Write's entire file content
or an Edit's replacement text would otherwise bury the very line that is
supposed to summarize the call.")

(defparameter *verbose-result-width* 160
  "How much of a tool's output is shown. Same reasoning as
*VERBOSE-VALUE-WIDTH*: a Read of a long file answers with the whole file.")

(defparameter *tool-input-key-order*
  '("command" "file_path" "pattern" "glob" "path" "url" "query" "prompt"
    "description" "subagent_type" "old_string" "new_string" "content")
  "Argument names printed first, in this order, ahead of every other argument
alphabetically. Hash table iteration order is unspecified in Common Lisp, so
without a fixed order the same call could list its arguments differently from
one run to the next, and the argument that actually identifies the call --
which file, which command -- would not reliably come first.")

(defun one-line (string width)
  "STRING as a single line of at most WIDTH characters: terminal control
characters stripped first (tool arguments and results are model/tool output,
every bit as untrusted as the model's own text), runs of whitespace --
including the newlines of a multi-line shell command or of file content --
collapsed to a single space, and whatever is left cut off with an ellipsis.
Staying on one line is what keeps a tool argument from swamping the trace,
and it also sidesteps the grey-across-newlines problem PRINT-SUBAGENT-BLOCK
documents, since there is never a newline inside the coloured span."
  (let ((flat (string-right-trim
               " "
               (with-output-to-string (out)
                 ;; Starting out already "inside" whitespace also drops any
                 ;; leading blank lines, which heredocs and file contents
                 ;; routinely carry.
                 (loop with previous-space = t
                       for ch across (strip-terminal-control-chars string)
                       for space = (member ch '(#\Space #\Tab #\Newline))
                       do (if space
                              (unless previous-space (write-char #\Space out))
                              (write-char ch out))
                          (setf previous-space (and space t)))))))
    (if (> (length flat) width)
        (concatenate 'string (subseq flat 0 width) "…")
        flat)))

(defun render-value (value)
  "A tool argument rendered as text: strings as they stand, anything else
(numbers, booleans, nested objects and arrays) written back out as JSON.
shasht parses JSON true/false/null as the keywords :TRUE/:FALSE/:NULL, which
~a would print as \":TRUE\"; sending them back through shasht shows them as
the tool was actually handed them."
  (if (stringp value)
      value
      (with-output-to-string (s) (shasht:write-json value s))))

(defun tool-input-keys (input)
  "INPUT's argument names, *TOOL-INPUT-KEY-ORDER* first and all the rest
alphabetically."
  (sort (hash-table-keys input)
        (lambda (a b)
          (let ((rank-a (position a *tool-input-key-order* :test #'equal))
                (rank-b (position b *tool-input-key-order* :test #'equal)))
            (cond ((and rank-a rank-b) (< rank-a rank-b))
                  (rank-a t)
                  (rank-b nil)
                  (t (string< a b)))))))

(defun tool-arg-strings (input width)
  "A tool_use's arguments as a list of key=\"value\" strings, each flattened
onto one line and cut off at WIDTH. This is the terminal trace's rendering;
the history file keeps the lines a value actually has, see TOOL-ARG-LINES."
  (when (hash-table-p input)
    (loop for key in (tool-input-keys input)
          collect (format nil "~a=\"~a\"" key
                          (one-line (render-value (gethash key input)) width)))))

(defun format-tool-use (block)
  "One line describing a tool_use BLOCK: the tool's name, then its arguments
as key=\"value\" pairs."
  (format nil "~a~{  ~a~}"
          (gethash "name" block)
          (tool-arg-strings (gethash "input" block) *verbose-value-width*)))

(defun tool-result-text (block)
  "The text a tool_result BLOCK carries. Its \"content\" is a plain string
for most tools, but an array of content blocks when a tool answers in several
parts (text alongside an image, say), of which only the text parts can be
shown on a line."
  (let ((content (gethash "content" block)))
    (cond
      ((stringp content) content)
      ((vectorp content)
       (format nil "~{~a~^ ~}"
               (loop for part across content
                     when (and (hash-table-p part)
                               (equal (gethash "type" part) "text"))
                       collect (gethash "text" part))))
      (t ""))))

(defun json-true-p (value)
  "True for a JSON true, which shasht parses as the keyword :TRUE rather than
as T."
  (and (member value '(:true t)) t))

;;; --- tool history ---------------------------------------------------------
;;; Every tool the CLI runs is also written down, as it happens, in a markdown
;;; file: what the terminal trace shows scrolls past and is gone, and with
;;; *VERBOSE* off it was never shown at all, so there was no way afterwards to
;;; ask what a turn actually did. Recorded independently of *VERBOSE* -- that
;;; flag decides what gets printed, not what gets kept -- and truncated at the
;;; start of every RUN, since this is a record of the turn in progress rather
;;; than a log that grows without bound.

(defparameter *tool-history-value-width* 2000
  "How much of a tool argument the history file keeps. Far wider than
*VERBOSE-VALUE-WIDTH*: that one has a terminal line to fit inside, whereas
here the point is to be able to read back the command that actually ran.
Still bounded, so a Write of a large file can't run away with the file.")

(defparameter *tool-history-result-width* 2000
  "How much of a tool's output the history file keeps. Same reasoning as
*TOOL-HISTORY-VALUE-WIDTH*.")

(defun tool-history-time (&optional (format '((:hour 2) #\: (:min 2) #\: (:sec 2))))
  "Now, rendered in *TIMEZONE* like the usage report's reset times, so a
history entry can be lined up against what was on screen."
  (local-time:format-timestring
   nil (local-time:now) :format format
   :timezone (or (find-timezone *timezone*) local-time:+utc-zone+)))

(defun append-tool-history (entry)
  "Append ENTRY to TOOLS-FILE, followed by the blank line that keeps it
separate from the next one. Failures are swallowed on purpose: the history
is a side record, and an unwritable /agent/data should not take down the
turn that is busy producing the output the user actually asked for."
  (ignore-errors
    (with-open-file (out TOOLS-FILE :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
      (format out "~a~%~%" entry))))

(defun reset-tool-history ()
  "Truncate TOOLS-FILE back to just its heading, so a run's history holds
that run's tools and nothing from the one before."
  (ignore-errors
    (with-open-file (out TOOLS-FILE :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
      (format out "# Claude Code tool history~%~%Run started ~a.~%~%"
              (tool-history-time '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
                                   (:hour 2) #\: (:min 2) #\: (:sec 2) #\Space :timezone))))))

(defparameter *tool-history-max-lines* 60
  "How many lines of a single command or result the history keeps. The point
of the file is to be able to read back what a turn did, and a Read of a long
file or a chatty test run would otherwise bury that under thousands of lines
of someone else's output.")

(defun split-lines (string)
  "STRING's lines, each right-trimmed of trailing whitespace, with leading and
trailing blank lines dropped -- tool output routinely arrives wrapped in
them, and in the history they would only push the next entry away from its
headline."
  (let ((lines (loop with start = 0
                     for newline = (position #\Newline string :start start)
                     collect (string-right-trim '(#\Space #\Tab #\Return)
                                                (subseq string start (or newline (length string))))
                     while newline
                     do (setf start (1+ newline)))))
    (loop while (and lines (zerop (length (first lines))))
          do (pop lines))
    (loop while (and lines (zerop (length (car (last lines)))))
          do (setf lines (butlast lines)))
    lines))

(defun history-lines (string width &optional (max-lines *tool-history-max-lines*))
  "STRING as the body lines of a history entry: kept as the several lines it
really has, since a shell heredoc or a directory listing rolled into one line
is exactly the output nobody can read. Bounded all the same -- at most
MAX-LINES lines and WIDTH characters across them all -- because a tool can
answer with a whole file. What gets cut is announced on a line of its own
rather than vanishing, so the file never quietly misrepresents what a tool
said."
  (let ((budget width)
        (kept '()))
    (loop for rest on (split-lines (strip-terminal-control-chars string))
          for line = (first rest)
          for index from 0
          do (cond
               ((or (>= index max-lines) (not (plusp budget)))
                (push (format nil "… (~a more line~:p)" (length rest)) kept)
                (return))
               ((> (length line) budget)
                (push (concatenate 'string (subseq line 0 budget) "…") kept)
                (setf budget 0))
               (t
                (push line kept)
                (decf budget (length line)))))
    (nreverse kept)))

(defun tool-history-entry (headline body-lines)
  "One history entry: a HEADLINE saying what happened -- when, which
subagent, which tool, and the call's own description when it has one --
then BODY-LINES on the lines straight below it, the actual command or
output, indented by four spaces and otherwise left exactly as the tool
wrote them: no quotes around them, no fence, nothing to read past. Keeping
headline and body apart is what makes the file skimmable -- the headlines
read as a narrative of the turn, with the bulky part sitting underneath.
The only blank line in an entry is the one APPEND-TOOL-HISTORY puts after
it, so each call reads as a single block."
  (format nil "~a~%~{    ~a~^~%~}" headline body-lines))

(defun tool-value-lines (value &optional indent)
  "One argument's value as body lines, bounded like every other body and
shifted right by INDENT when it is sitting underneath its own name."
  (let ((lines (history-lines (render-value value) *tool-history-value-width*)))
    (if indent
        (mapcar (lambda (line) (concatenate 'string indent line)) lines)
        lines)))

(defun tool-description (block)
  "A tool_use BLOCK's own description argument on one line, or NIL when the
tool takes none. Both the call's entry and its result's repeat it: a long
body can sit between the two, and a bare \"Bash ⤶ result\" halfway down the
file says nothing about which piece of work answered."
  (let* ((input (gethash "input" block))
         (description (and (hash-table-p input) (gethash "description" input))))
    (and description
         (one-line (render-value description) *tool-history-value-width*))))

(defun record-tool-use (subagent-name block)
  "Write a tool call down: who called what, with the call's own description
when the tool takes one (Bash and Agent do), then its arguments. A call whose
only argument is a command is written as the bare command -- the headline
already says the tool was Bash, so a \"command:\" above it would be noise --
while a call carrying anything else names every argument it prints and sets
its value underneath."
  (let* ((input (gethash "input" block))
         ;; "description" is left out of the body: it is already the headline.
         (keys (and (hash-table-p input)
                    (remove "description" (tool-input-keys input) :test #'equal)))
         (body (cond
                 ((null keys) (list "(no arguments)"))
                 ((equal keys '("command"))
                  (tool-value-lines (gethash "command" input)))
                 (t (loop for key in keys
                          append (cons (format nil "~a:" key)
                                       (tool-value-lines (gethash key input) "  ")))))))
    (append-tool-history
     (tool-history-entry
      (format nil "`~a` ~@[*~a* ~]**~a**~@[ — ~a~]"
              (tool-history-time) subagent-name (gethash "name" block)
              (tool-description block))
      body))))

(defun record-tool-result (subagent-name tool-name description block)
  "Write what a tool answered down as its own entry. It sits under the call
it answers by position rather than by nesting -- results arrive after their
call, and parallel calls interleave -- so the headline repeats the tool's
name and DESCRIPTION to say which call came back."
  (let ((lines (history-lines (tool-result-text block) *tool-history-result-width*)))
    (append-tool-history
     (tool-history-entry
      (format nil "`~a` ~@[*~a* ~]**~a** ⤶ ~:[result~;failed~]~@[ — ~a~]"
              (tool-history-time) subagent-name tool-name
              (json-true-p (gethash "is_error" block))
              description)
      (or lines (list "(no output)"))))))

(defun make-stream-printer ()
  "Return a fresh ON-EVENT callback for CALL-CLAUDE that renders a
stream_event live: dim grey for thinking, plain for the reply text, and --
when *VERBOSE* is on -- a grey line per tool call and per tool result. Ensures
exactly one blank line separates each transition into a text block —
thinking -> answering, but also text -> tool call -> text when the model
keeps talking after using a tool — regardless of how many newlines the
model's own content happens to carry across that boundary. Tracked via the
count of trailing newlines already written (capped at 2) rather than an
unconditional insert, since always inserting one (the previous approach)
double-spaced whenever the model's own text already supplied the gap, and
inserting only once ever under-spaced every later transition."
  (let ((trailing-newlines 2) ; RUN's own preamble already ends on a blank line
        (pending-bracket "")
        ;; Task tool_use id -> subagent name (its "description", falling back
        ;; to "subagent_type"), learned from the top-level assistant's own
        ;; tool_use blocks so subagent output can be tagged with which
        ;; subagent it came from instead of a generic "[subagent]" label.
        (subagent-names (make-hash-table :test #'equal))
        ;; tool_use id -> tool name, so a tool_result -- which carries only
        ;; the id it answers -- can say which tool it came back from, and
        ;; id -> that call's description, so it can say which call too.
        (tool-names (make-hash-table :test #'equal))
        (tool-descriptions (make-hash-table :test #'equal))
        ;; Whether the last thing written was a verbose trace line, so a run
        ;; of them stays single spaced (see PRINT-TRACE-LINE).
        (last-line-was-trace nil))
    (labels ((track! (str)
               (loop for ch across str
                     do (setf trailing-newlines (if (char= ch #\Newline) (min 2 (1+ trailing-newlines)) 0))))
             (ensure-blank-line ()
               (loop while (< trailing-newlines 2)
                     do (write-char #\Newline)
                        (incf trailing-newlines)))
             (ensure-line-start ()
               (when (zerop trailing-newlines)
                 (write-char #\Newline)
                 (incf trailing-newlines)))
             (reconstruct-and-buffer (str)
               "Like RECONSTRUCT-MISSING-SGR-ESCAPES, but a bare SGR sequence
can itself be split across delta chunks (\"[3\" then \"3m\") just like
anything else in a stream. A trailing '[' still plausibly mid-sequence
(only digits/semicolons so far, no terminator) is held in PENDING-BRACKET
and prepended to the next chunk instead of being emitted -- and reset --
early, unfixed."
               (let* ((full (concatenate 'string pending-bracket str))
                      (len (length full)))
                 (setf pending-bracket "")
                 (with-output-to-string (out)
                   (loop with i = 0
                         while (< i len)
                         do (if (char= (char full i) #\[)
                                (let ((end (loop for j from (1+ i) below len
                                                  for ch = (char full j)
                                                  do (cond
                                                       ((or (digit-char-p ch) (char= ch #\;)))
                                                       ((char= ch #\m) (return (1+ j)))
                                                       (t (return nil)))
                                                  finally (return :incomplete))))
                                  (cond
                                    ((eq end :incomplete)
                                     (setf pending-bracket (subseq full i))
                                     (setf i len))
                                    (end
                                     (unless (and (plusp i) (char= (char full (1- i)) #\Escape))
                                       (write-char #\Escape out))
                                     (write-string full out :start i :end end)
                                     (setf i end))
                                    (t
                                     (write-char (char full i) out)
                                     (incf i))))
                                (progn
                                  (write-char (char full i) out)
                                  (incf i)))))))
             (grey-across-newlines (string)
               "Like GREY, but safe for a chunk that may itself contain an
embedded newline (thinking_delta chunks routinely do, e.g. at a paragraph
break) -- wrapping such a chunk in one GREY call would put the SGR reset
after that newline, which rlwrap renders in the terminal's default color
instead of grey, the same failure PRINT-SUBAGENT-BLOCK's grey span already
works around by keeping start/reset on the same side of every newline."
               (let ((start 0) (len (length string)))
                 (with-output-to-string (out)
                   (loop
                     (let ((nl (position #\Newline string :start start)))
                       (write-string (grey (subseq string start (or nl len))) out)
                       (unless nl (return))
                       (write-char #\Newline out)
                       (setf start (1+ nl))
                       (when (>= start len) (return)))))))
             (flush-pending! ()
               "A held-back PENDING-BRACKET never completes if the block
ends right there -- emit it as literal, unfixed text rather than losing it."
               (unless (zerop (length pending-bracket))
                 (write-string pending-bracket)
                 (setf pending-bracket "")))
             (print-subagent-block (content-key subagent-name block)
               "A --forward-subagent-text block arrives as a single
complete string (not incremental deltas like the main agent's own
content), so there's no chunk-boundary/SGR-buffering concern here -- just
sanitize and print it as one unit, tagged with which subagent it came
from so it's visually distinct from the main agent's own narration. The
GREY-wrapped span deliberately excludes the trailing newline -- when it
was included, the SGR start code and its reset ended up either side of
a \\n inside one write, and rlwrap (which agent-run.sh pipes the REPL
through for readline editing) mishandles color resets that land past a
line boundary like that, so the line rendered in the terminal's default
white instead of grey."
               (let ((clean (strip-terminal-control-chars (gethash content-key block))))
                 (unless (zerop (length clean))
                   (flush-pending!)
                   (ensure-blank-line)
                   (let ((line (format nil "  ⤷ [~a] ~a" subagent-name clean)))
                     (write-string (grey line))
                     (write-char #\Newline)
                     (track! (concatenate 'string line (string #\Newline))))
                   (setf last-line-was-trace nil)
                   (finish-output))))
             (print-trace-line (line)
               "One grey line of the *VERBOSE* trace. The first one after any
other output gets the usual blank line separating it from the text above,
but a run of them -- the common case, a model firing several tools back to
back -- stays single spaced instead of double spacing the whole trace. LINE
is always newline-free (see ONE-LINE), so the coloured span and its reset
stay on the same side of the line break, which is the rlwrap constraint
PRINT-SUBAGENT-BLOCK documents."
               (flush-pending!)
               (if last-line-was-trace (ensure-line-start) (ensure-blank-line))
               (write-string (grey line))
               (write-char #\Newline)
               (track! (concatenate 'string line (string #\Newline)))
               (setf last-line-was-trace t)
               (finish-output))
             (print-tool-use (subagent-name block)
               "Record a tool call in the tool history and, when *VERBOSE* is
on, echo it: which tool, and what it was handed. Also records the call's id
so PRINT-TOOL-RESULT can name the tool its result belongs to. The history is
written either way -- *VERBOSE* governs the terminal, not the file."
               (setf (gethash (gethash "id" block) tool-names) (gethash "name" block)
                     (gethash (gethash "id" block) tool-descriptions) (tool-description block))
               (record-tool-use subagent-name block)
               (when *verbose*
                 (print-trace-line (format nil "  ⚒ ~@[[~a] ~]~a"
                                           subagent-name (format-tool-use block)))))
             (print-tool-result (subagent-name block)
               "Record what a tool answered, and when *VERBOSE* is on echo it
too, under the name of the tool that was called -- a tool_result block itself
only carries the tool_use id."
               (let ((tool-name (or (gethash (gethash "tool_use_id" block) tool-names) "tool")))
                 (record-tool-result subagent-name tool-name
                                     (gethash (gethash "tool_use_id" block) tool-descriptions)
                                     block)
                 (when *verbose*
                   (let ((text (one-line (tool-result-text block) *verbose-result-width*)))
                     (print-trace-line
                      (format nil "  ⤶ ~@[[~a] ~]~a~:[~; failed~]: ~a"
                              subagent-name tool-name
                              (json-true-p (gethash "is_error" block))
                              (if (zerop (length text)) "(no output)" text)))))))
             (remember-subagent-name (block)
               "Learn a subagent-spawning tool_use's id -> name mapping, so
--forward-subagent-text blocks tagged with that id can be labelled with the
subagent they came from rather than a generic \"subagent\". This CLI build
calls that tool \"Agent\" (older/other builds call it \"Task\") -- match
either name so this doesn't silently stop naming subagents if it ever
changes back."
               (when (member (gethash "name" block) '("Agent" "Task") :test #'equal)
                 (let* ((input (gethash "input" block))
                        (name (and (hash-table-p input)
                                   (or (gethash "description" input)
                                       (gethash "subagent_type" input)))))
                   (when name
                     (setf (gethash (gethash "id" block) subagent-names) name))))))
      (lambda (event)
        (let ((event-type (gethash "type" event)))
          (cond
            ((equal event-type "stream_event")
             (sb-thread:with-mutex (*output-lock*)
               (let* ((inner (gethash "event" event))
                      (inner-type (gethash "type" inner)))
                 (cond
                   ((and (equal inner-type "content_block_start")
                         (equal (gethash "type" (gethash "content_block" inner)) "text"))
                    (flush-pending!)
                    (ensure-blank-line)
                    (setf last-line-was-trace nil)
                    (finish-output))
                   ((equal inner-type "content_block_stop")
                    (flush-pending!)
                    (finish-output))
                   ((equal inner-type "content_block_delta")
                    (let* ((delta (gethash "delta" inner))
                           (delta-type (gethash "type" delta))
                           (clean (cond
                                    ((equal delta-type "thinking_delta")
                                     (strip-terminal-control-chars (gethash "thinking" delta)))
                                    ((equal delta-type "text_delta")
                                     (strip-terminal-control-chars
                                      (reconstruct-and-buffer (gethash "text" delta)))))))
                      ;; Many thinking_delta chunks arrive genuinely empty; skip
                      ;; the write+flush entirely rather than doing a no-op
                      ;; syscall for invisible content on every single one.
                      (when (and clean (plusp (length clean)))
                        (write-string (if (equal delta-type "thinking_delta") (grey-across-newlines clean) clean))
                        (track! clean)
                        (setf last-line-was-trace nil)
                        (finish-output))))))))
            ;; The CLI's opening event, which says which session, model and
            ;; working directory the turn actually got -- the "where" every
            ;; relative path in the trace below is relative to.
            ((and *verbose*
                  (equal event-type "system")
                  (equal (gethash "subtype" event) "init"))
             (sb-thread:with-mutex (*output-lock*)
               (let ((tools (gethash "tools" event)))
                 (print-trace-line
                  (format nil "  ⚙ init~@[ · model ~a~]~@[ · cwd ~a~]~@[ · ~a tools~]"
                          (gethash "model" event)
                          (gethash "cwd" event)
                          (and (vectorp tools) (length tools)))))))
            ;; Whole (non-partial) messages, which is where tool calls and
            ;; their results show up -- and, with --forward-subagent-text,
            ;; where a subagent's own text/thinking is relayed as a top-level
            ;; message tagged with the id of the Task/Agent tool_use that
            ;; spawned it, instead of leaving subagent-heavy turns silent.
            ;; shasht parses JSON null as the truthy keyword :NULL, not NIL --
            ;; without excluding it explicitly, the top-level's own messages
            ;; (which carry an explicit "parent_tool_use_id":null) would
            ;; wrongly look like forwarded subagent output.
            ((member event-type '("assistant" "user") :test #'equal)
             (sb-thread:with-mutex (*output-lock*)
               (let* ((parent (gethash "parent_tool_use_id" event))
                      (blocks (or (gethash "content" (gethash "message" event)) #()))
                      ;; NIL for the top-level agent's own messages, the
                      ;; subagent's name for a forwarded one.
                      (subagent-name (when (and parent (not (eq parent :null)))
                                       (or (gethash parent subagent-names) "subagent"))))
                 (loop for block across blocks
                       for block-type = (gethash "type" block)
                       do (cond
                            ;; The top-level agent's own text and thinking
                            ;; already streamed in as deltas above; only a
                            ;; subagent's arrives whole, here.
                            ((and subagent-name
                                  (member block-type '("text" "thinking") :test #'equal))
                             (print-subagent-block block-type subagent-name block))
                            ((equal block-type "tool_use")
                             (remember-subagent-name block)
                             (print-tool-use subagent-name block))
                            ((equal block-type "tool_result")
                             (print-tool-result subagent-name block)))))))))))))

(defun drain-stderr (process)
  "Forward the child process's stderr to *error-output*, sanitizing each
line first. Runs on its own thread so it can drain concurrently with the
main stdout loop — :error t would inherit the terminal directly and let
the CLI's own raw progress/status output (e.g. \\r-redraws) bypass
sanitization entirely."
  (loop for line = (ignore-errors (read-line (sb-ext:process-error process) nil nil))
        while line
        do (sb-thread:with-mutex (*output-lock*)
             (format *error-output* "[error] ~a~%" (strip-terminal-control-chars line))
             (finish-output *error-output*))))

(defun call-claude (prompt on-event)
  "Run the claude CLI on PROMPT, calling ON-EVENT with each parsed JSON event
as it arrives so the caller can render thinking/text while the CLI is still
working instead of waiting for the process to exit.
Returns (values answer-text session-id total-cost-usd rate-limit-info usage)."
  (let* ((session-id (read-session-id))
         (args (append (list "-p" prompt
                              "--output-format" "stream-json"
                              "--include-partial-messages"
                              "--verbose"
                              "--model" *model*
                              "--permission-mode" *permission-mode*
                              ;; The CLI can write its own decorative UI (spinners,
                              ;; borders, animations) straight to the terminal,
                              ;; bypassing our stdout/stderr pipes entirely — this
                              ;; flag turns that off at the source.
                              "--ax-screen-reader"
                              ;; Without this, a subagent-heavy turn (Task tool)
                              ;; is completely silent until the subagent finishes;
                              ;; this surfaces its text/thinking as it happens.
                              "--forward-subagent-text")
                       (when *effort* (list "--effort" *effort*))
                       (when session-id (list "--resume" session-id))))
         (process (sb-ext:run-program *claude-bin* args
                                       :output :stream :error :stream
                                       :external-format '(:utf-8 :replacement #\?)
                                       :wait nil :search t))
         (result nil)
         (rate-limit nil))
    (sb-thread:make-thread (lambda () (drain-stderr process)) :name "claude-stderr")
    (loop for line = (read-line (sb-ext:process-output process) nil nil)
          while line
          unless (zerop (length line))
            do (ignore-errors
                 (let* ((event (shasht:read-json line))
                        (type (gethash "type" event)))
                   (funcall on-event event)
                   (cond
                     ((equal type "rate_limit_event")
                      (setf rate-limit (gethash "rate_limit_info" event)))
                     ((equal type "result") (setf result event))))))
    (sb-ext:process-wait process)
    (values (gethash "result" result)
            (gethash "session_id" result)
            (gethash "total_cost_usd" result)
            rate-limit
            (gethash "usage" result))))

;;; --- usage reporting ---------------------------------------------------
;;; Rendered just above the separator so every reply shows where the
;;; account stands on the rolling 5h/7d rate-limit windows and what the
;;; call cost, in dollars.

(defun format-reset-time (epoch-seconds)
  "Reset instant rendered in *TIMEZONE*, e.g. \"2026-09-23 03:59 CEST\".
Falls back to UTC when *TIMEZONE* names no known zone."
  (local-time:format-timestring
   nil (local-time:unix-to-timestamp epoch-seconds)
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
             (:hour 2) #\: (:min 2) #\Space :timezone)
   :timezone (or (find-timezone *timezone*) local-time:+utc-zone+)))

(defun format-remaining (epoch-seconds)
  "Human-readable countdown from now until EPOCH-SECONDS, e.g. \"2d 3h\" or
\"45m\"."
  (let* ((now (- (get-universal-time) +UNIX-EPOCH-UNIVERSAL-TIME+))
         (remaining (max 0 (- epoch-seconds now)))
         (days (floor remaining 86400))
         (hours (floor (mod remaining 86400) 3600))
         (minutes (floor (mod remaining 3600) 60)))
    (cond
      ((plusp days) (format nil "~dd ~dh" days hours))
      ((plusp hours) (format nil "~dh ~dm" hours minutes))
      (t (format nil "~dm" minutes)))))

(defun format-window (label window)
  (when window
    (let ((utilization (gethash "utilization" window))
          (resets-at (gethash "resetsAt" window)))
      (format nil "~a: ~,1f% used - resets ~a (in ~a)"
              label (* 100 utilization)
              (format-reset-time resets-at)
              (format-remaining resets-at)))))

(defun format-windows (rate-limit)
  "The Session(5h)/Week(7d) lines alone, empty string when RATE-LIMIT is nil.
Shared between FORMAT-USAGE (after every real call) and USAGE (which has no
rate-limit data of its own, since /usage is answered locally by the CLI)."
  (let* ((windows (and rate-limit (gethash "unifiedWindows" rate-limit)))
         (five-hour (format-window "Session (5h)" (and windows (gethash "five_hour" windows))))
         (seven-day (format-window "Week (7d)" (and windows (gethash "seven_day" windows)))))
    (with-output-to-string (s)
      (when five-hour (format s "~a~%" five-hour))
      (when seven-day (format s "~a~%" seven-day)))))

(defun format-tokens (usage)
  "One line of per-call token counts from a result event's USAGE hash table:
input/output tokens plus cache read/creation tokens when present."
  (when usage
    (let ((input (gethash "input_tokens" usage))
          (output (gethash "output_tokens" usage))
          (cache-read (gethash "cache_read_input_tokens" usage))
          (cache-creation (gethash "cache_creation_input_tokens" usage)))
      (format nil "Tokens: ~a in, ~a out~@[, ~a cache read~]~@[, ~a cache creation~]"
              (or input 0) (or output 0)
              (and cache-read (plusp cache-read) cache-read)
              (and cache-creation (plusp cache-creation) cache-creation)))))

(defun format-usage (cost rate-limit usage)
  (let ((tokens (format-tokens usage)))
    (with-output-to-string (s)
      (write-string (format-windows rate-limit) s)
      (when tokens (format s "~a~%" tokens))
      (format s "Cost: $~,4f this session" (or cost 0)))))

;;; --- entry point ------------------------------------------------------------

(defun use () nil)

(defun usage ()
  "Print the claude CLI's own /usage report (subscription session/week limits
and usage breakdown). /usage is answered locally by the CLI, not by the
model, so this costs nothing and doesn't touch the conversation history.
Also prints the exact reset countdown from the last real call, when one
has happened this session, since /usage's own text has no such payload."
  (multiple-value-bind (text) (call-claude "/usage" (lambda (event) (declare (ignore event))))
    (sb-thread:with-mutex (*output-lock*)
      (format t "~&~a~%" (strip-terminal-control-chars text))
      (let ((windows (format-windows *last-rate-limit*)))
        (when (plusp (length windows))
          (format t "~&~a" (grey windows)))))))

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (reset-tool-history)
  (format t "~&______~&~%")
  (multiple-value-bind (text session-id cost rate-limit usage)
      (call-claude prompt (make-stream-printer))
    (when session-id (write-session-id session-id))
    (when rate-limit (setf *last-rate-limit* rate-limit))
    (remember (append (recall)
                      (list (obj "role" "user" "content" prompt)
                            (obj "role" "assistant" "content" (strip-terminal-control-chars text)))))
    (sb-thread:with-mutex (*output-lock*)
      (format t "~&~%~a~%~a ~a~%"
			  (grey (format-usage cost rate-limit usage))
			  SEP (grey *model*)))
    (set-status STATUS-OK)))

(defun use ()
  (setf *current-run-fn* #'run
        *current-model* *model*
        *memory-file* (pathname MEMORY-FILE)
        *system-message* '())
  (set-status STATUS-OK)
  *model*)

(defun forget ()
  (let ((*memory-file* (pathname MEMORY-FILE)))
    (forget-mem))
  (when (probe-file SESSION-FILE) (delete-file SESSION-FILE)))

(defun list-models ()
  (loop for name across *models*
        for index from 1
        do (format t "~&[~a] ~a~%" index name)))

(defun lm () (list-models))

(defun set-model (num)
  (list-models)
  (setf *model* (aref *models* (- num 1)))
  (use))

(defun list-efforts ()
  (loop for name across *efforts*
        for index from 1
        do (format t "~&[~a] ~a~%" index name)))

(defun le () (list-efforts))

(defun set-effort (num)
  (list-efforts)
  (setf *effort* (aref *efforts* (- num 1)))
  (use)
  *effort*)

(defun set-timezone (name)
  "Set the timezone usage reset times are shown in, e.g.
\(set-timezone \"Asia/Tokyo\"). Unknown names are rejected rather than stored,
since rendering would silently fall back to UTC later on."
  (if (find-timezone name)
      (setf *timezone* name)
      (progn
        (format t "~&Unknown timezone: ~a (keeping ~a)~%" name *timezone*)
        *timezone*)))

(defun set-verbose (&optional (on (not *verbose*)))
  "Turn the verbose tool trace on or off; called with no argument it toggles,
so (set-verbose) flips it and (set-verbose nil) forces it off. With it on,
every tool the CLI runs prints a grey line as it happens -- the tool's name
and arguments when it starts, a preview of its output when it answers, both
tagged with the subagent's name when the call came from one -- so a long turn
shows what it is actually doing instead of going quiet between paragraphs."
  (setf *verbose* (and on t))
  (format t "~&Verbose: ~:[off~;on~]~%" *verbose*)
  *verbose*)

(defun verbose (&optional (on (not *verbose*)))
  "Shorthand for SET-VERBOSE, to type at the REPL."
  (set-verbose on))
