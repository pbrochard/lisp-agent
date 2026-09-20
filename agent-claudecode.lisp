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
(defparameter *model* "sonnet")
(defparameter *permission-mode* "bypassPermissions")

;;; The CLI rides subscription auth and has no models endpoint to ask, so the
;;; aliases --model accepts are hard-coded.
(defparameter *models* (vector "sonnet" "opus" "fable" "haiku"))

(defparameter *effort* nil)
(defparameter *efforts* (vector "low" "medium" "high" "xhigh" "max"))

;;; stdout (streamed deltas, main thread) and stderr (DRAIN-STDERR, its own
;;; thread) share one terminal; without this lock their writes interleave
;;; mid-line and look like a garbled, truncated line.
(defparameter *output-lock* (sb-thread:make-mutex :name "claude-output"))

(defparameter *last-rate-limit* nil
  "rate_limit_info from the last real call, kept because /usage is answered
locally by the CLI and carries no rate-limit payload of its own.")

(defparameter *verbose* t
  "Whether every tool the CLI runs is echoed as it happens. See SET-VERBOSE.")

(defparameter *timezone* "Europe/Paris"
  "IANA zone the rate-limit reset times are rendered in, resolved against
local-time's bundled zoneinfo. A fixed UTC offset would be wrong half the
year anywhere that keeps DST.")

(defparameter *timezone-repository-loaded* nil)

(defun load-timezone-repository-once ()
  "local-time answers NIL for every zone name until its repository is read,
rather than complaining, so read it on first use -- it costs ~50ms."
  (unless *timezone-repository-loaded*
    (local-time:reread-timezone-repository)
    (setf *timezone-repository-loaded* t)))

(defun find-timezone (name)
  "The local-time timezone object for IANA NAME, or NIL if there is no such zone."
  (load-timezone-repository-once)
  (ignore-errors (local-time:find-timezone-by-location-name name)))

(defun display-timezone ()
  (or (find-timezone *timezone*) local-time:+utc-zone+))

(defconstant MEMORY-FILE "/agent/data/memory-claudecode.json")
(defconstant SESSION-FILE "/agent/data/session-claudecode.txt")
(defconstant TOOLS-FILE "/agent/data/claudecode-tools.md")

;;; --- session id persistence ---------------------------------------------
;;; The CLI keeps the transcript itself; all we carry across processes is
;;; which session id to --resume.

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
;;; The "call" is a subprocess in print mode, JSON in, JSON out.

(defconstant +UNIX-EPOCH-UNIVERSAL-TIME+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun c0-control-but-newline-or-tab-p (code)
  (and (< code 32) (not (member code '(9 10)))))

(defun delete-char-p (code)
  (= code 127))

(defun c1-control-p (code)
  "The 8-bit equivalents of the ESC-introduced CSI/OSC sequences some
terminals honor."
  (<= #x80 code #x9F))

(defun bidi-formatting-char-p (code)
  "Embeddings, overrides and isolates, which a terminal honoring bidi can be
made to visually reorder displayed text with."
  (or (<= #x202A code #x202E)
      (<= #x2066 code #x2069)))

(defun unsafe-terminal-char-p (ch)
  "True for a character that could rewrite, erase or visually spoof
already-printed terminal output. Newlines and tabs are left alone."
  (let ((code (char-code ch)))
    (or (delete-char-p code)
        (c1-control-p code)
        (bidi-formatting-char-p code)
        (c0-control-but-newline-or-tab-p code))))

(defun sgr-parameters-end (string start)
  "The index just past the #\\m closing the SGR parameters that begin at
START (the #\\[), NIL when something else is there, or :INCOMPLETE when the
string runs out while the sequence could still be well-formed."
  (loop for i from (1+ start) below (length string)
        for ch = (char string i)
        do (cond
             ((or (digit-char-p ch) (char= ch #\;)))
             ((char= ch #\m) (return (1+ i)))
             (t (return nil)))
        finally (return :incomplete)))

(defun sgr-sequence-end (string start)
  "The index just past a complete SGR sequence ESC[<params>m starting at
START, or NIL. SGR is the one escape shape that only changes how later text
is rendered -- it cannot move the cursor, erase anything or touch the screen
-- so it is safe to let through even though model output is untrusted."
  (when (and (< (1+ start) (length string))
             (char= (char string (1+ start)) #\[))
    (let ((end (sgr-parameters-end string (1+ start))))
      (and (integerp end) end))))

(defun strip-terminal-control-chars (string)
  "STRING with every UNSAFE-TERMINAL-CHAR-P character removed and every
well-formed SGR colour sequence left intact. Model output is untrusted and
must not be able to manipulate the terminal it is printed to. STRING itself
is returned when nothing needs stripping -- the common case on the streaming
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
;;; Each tool the CLI runs arrives as a tool_use content block and what it
;;; answered as a tool_result block in the message after; with *VERBOSE* on
;;; each becomes one grey line between the model's own paragraphs.

(defparameter *verbose-value-width* 160
  "How much of a single tool argument the trace shows.")

(defparameter *verbose-result-width* 160
  "How much of a tool's output the trace shows.")

(defparameter *tool-input-key-order*
  '("command" "file_path" "pattern" "glob" "path" "url" "query" "prompt"
    "description" "subagent_type" "old_string" "new_string" "content")
  "Argument names printed first, in this order, the rest alphabetically after
them. Hash table iteration order is unspecified in Common Lisp, so without a
fixed order the same call would list its arguments differently from one run
to the next.")

(defun collapse-whitespace (string)
  "STRING with every run of whitespace -- including the newlines of a
multi-line command or of file content -- squeezed to a single space, and no
leading or trailing space left."
  (string-right-trim
   " "
   (with-output-to-string (out)
     (loop with previous-space = t
           for ch across string
           for space = (member ch '(#\Space #\Tab #\Newline))
           do (if space
                  (unless previous-space (write-char #\Space out))
                  (write-char ch out))
              (setf previous-space (and space t))))))

(defun truncate-with-ellipsis (string width)
  (if (> (length string) width)
      (concatenate 'string (subseq string 0 width) "…")
      string))

(defun one-line (string width)
  "STRING sanitized, flattened onto one line and cut to WIDTH. Staying on one
line keeps a tool argument from swamping the trace, and keeps every coloured
span free of the newline that GREY-MULTILINE exists to work around."
  (truncate-with-ellipsis
   (collapse-whitespace (strip-terminal-control-chars string))
   width))

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
  "A tool_use's arguments as a list of key=\"value\" strings for the terminal
trace, each flattened onto one line and cut off at WIDTH. The history file
keeps the lines a value really has instead, see TOOL-VALUE-LINES."
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
  "The text a tool_result BLOCK carries: its \"content\" is a plain string for
most tools, and an array of content blocks when a tool answers in several
parts (text alongside an image, say), of which only the text can be shown."
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
  "True for a JSON true, which shasht parses as :TRUE rather than as T."
  (and (member value '(:true t)) t))

;;; --- tool history ---------------------------------------------------------
;;; The trace scrolls past and, with *VERBOSE* off, is never shown at all, so
;;; every tool is written down in a markdown file as the turn happens. The
;;; file is truncated by RESET-TOOL-HISTORY at the start of each RUN.

(defparameter *tool-history-value-width* 2000
  "How much of a tool argument the history file keeps. Far wider than
*VERBOSE-VALUE-WIDTH*, which has a terminal line to fit inside, but still
bounded so a Write of a large file cannot run away with the file.")

(defparameter *tool-history-result-width* 2000
  "How much of a tool's output the history file keeps.")

(defparameter *tool-history-max-lines* 60
  "How many lines of a single command or result the history keeps, so a Read
of a long file cannot bury the turn it is meant to describe.")

(defparameter +clock-format+ '((:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter +date-and-clock-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
    (:hour 2) #\: (:min 2) #\: (:sec 2) #\Space :timezone))

(defun tool-history-time (&optional (format +clock-format+))
  "Now, in *TIMEZONE*, so a history entry lines up with what was on screen."
  (local-time:format-timestring nil (local-time:now)
                                :format format
                                :timezone (display-timezone)))

(defmacro with-tool-history ((stream &key (if-exists :append)) &body body)
  "Run BODY with STREAM open on TOOLS-FILE. Failures are swallowed on
purpose: the history is a side record, and an unwritable /agent/data must not
take down the turn that is producing the output actually asked for."
  `(ignore-errors
     (with-open-file (,stream TOOLS-FILE :direction :output
                                         :if-exists ,if-exists
                                         :if-does-not-exist :create)
       ,@body)))

(defun append-tool-history (entry)
  "Append ENTRY, followed by the blank line separating it from the next one."
  (with-tool-history (out)
    (format out "~a~%~%" entry)))

(defun reset-tool-history ()
  "Truncate the history back to its heading, so a run's file holds that run's
tools and nothing from the one before."
  (with-tool-history (out :if-exists :supersede)
    (format out "# Claude Code tool history~%~%Run started ~a.~%~%"
            (tool-history-time +date-and-clock-format+))))

(defun split-lines (string)
  "STRING's lines, each right-trimmed, with leading and trailing blank lines
dropped -- tool output routinely arrives wrapped in them."
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

(defun lines-omitted-note (count)
  (format nil "… (~a more line~:p)" count))

(defun history-lines (string width &optional (max-lines *tool-history-max-lines*))
  "STRING as the body lines of a history entry, keeping the lines it really
has -- a heredoc or a listing rolled into one line is what nobody can read --
within MAX-LINES lines and WIDTH characters across them all. Whatever is cut
is announced, so the file never quietly misrepresents what a tool said."
  (let ((budget width)
        (kept '()))
    (loop for rest on (split-lines (strip-terminal-control-chars string))
          for line = (first rest)
          for index from 0
          do (cond
               ((or (>= index max-lines) (not (plusp budget)))
                (push (lines-omitted-note (length rest)) kept)
                (return))
               ((> (length line) budget)
                (push (truncate-with-ellipsis line budget) kept)
                (setf budget 0))
               (t
                (push line kept)
                (decf budget (length line)))))
    (nreverse kept)))

(defparameter *language-by-extension*
  '(("lisp" . "lisp") ("asd" . "lisp") ("cl" . "lisp") ("el" . "elisp")
    ("scm" . "scheme") ("clj" . "clojure")
    ("sh" . "bash") ("bash" . "bash") ("zsh" . "bash") ("fish" . "fish")
    ("py" . "python") ("rb" . "ruby") ("pl" . "perl") ("lua" . "lua")
    ("js" . "javascript") ("mjs" . "javascript") ("cjs" . "javascript")
    ("jsx" . "jsx") ("ts" . "typescript") ("tsx" . "tsx")
    ("json" . "json") ("yaml" . "yaml") ("yml" . "yaml") ("toml" . "toml")
    ("xml" . "xml") ("html" . "html") ("css" . "css") ("scss" . "scss")
    ("md" . "markdown") ("markdown" . "markdown") ("org" . "org")
    ("sql" . "sql") ("csv" . "csv") ("diff" . "diff") ("patch" . "diff")
    ("c" . "c") ("h" . "c") ("cpp" . "cpp") ("cc" . "cpp") ("hpp" . "cpp")
    ("go" . "go") ("rs" . "rust") ("java" . "java") ("kt" . "kotlin")
    ("php" . "php") ("swift" . "swift") ("r" . "r") ("ex" . "elixir")
    ("mk" . "makefile") ("dockerfile" . "dockerfile"))
  "File extension -> the name a markdown renderer highlights it under. A
missing extension costs nothing but plain text.")

(defun file-name-of (path)
  (subseq path (1+ (or (position #\/ path :from-end t) -1))))

(defun file-extension-of (name)
  (let ((dot (position #\. name :from-end t)))
    (and dot (subseq name (1+ dot)))))

(defun language-for-path (path)
  "The highlighting language for the file at PATH, from its extension or --
for the few carrying none -- from its whole name. NIL when PATH is not a
string or names nothing recognised."
  (when (stringp path)
    (let ((name (string-downcase (file-name-of path))))
      (cond
        ((string= name "dockerfile") "dockerfile")
        ((string= name "makefile") "makefile")
        (t (cdr (assoc (file-extension-of name) *language-by-extension*
                       :test #'equal)))))))

(defun longest-backtick-run (lines)
  (let ((longest 0))
    (dolist (line lines longest)
      (loop with run = 0
            for ch across line
            do (if (char= ch #\`)
                   (setf run (1+ run) longest (max longest run))
                   (setf run 0))))))

(defun fence-marker (lines)
  "A fence long enough that LINES cannot close the block early -- content
that is itself markdown, a README or this very file, otherwise spills out."
  (make-string (max 3 (1+ (longest-backtick-run lines))) :initial-element #\`))

(defun fenced (lines &optional language)
  "LINES as a markdown fenced code block, tagged with LANGUAGE when one is
known. Fenced rather than indented because an indented block may not
interrupt a paragraph, which left every body folded into its headline."
  (let ((fence (fence-marker lines)))
    (append (list (concatenate 'string fence (or language "")))
            lines
            (list fence))))

(defun tool-history-entry (headline body-lines)
  "A HEADLINE saying what happened, with the command or output on the lines
straight below it. The only blank line in an entry is the one
APPEND-TOOL-HISTORY puts after it, so each call reads as a single block."
  (format nil "~a~%~{~a~^~%~}" headline body-lines))

(defun tool-value-lines (value &optional language)
  "One argument's value as a fenced block, bounded like every other body."
  (fenced (history-lines (render-value value) *tool-history-value-width*) language))

(defun tool-arg-language (key value input)
  "What a renderer should highlight one argument as. Anything not clearly a
command, a structured value or file content is left plain: guessing wrong
colours a value as something it is not."
  (cond
    ((equal key "command") "bash")
    ((not (stringp value)) (and (or (hash-table-p value) (vectorp value)) "json"))
    ((member key '("content" "new_string" "old_string") :test #'equal)
     (language-for-path (gethash "file_path" input)))))

(defun tool-description (block)
  "A tool_use BLOCK's own description argument on one line, or NIL when the
tool takes none. The call's entry and its result's both carry it, since a
long body can sit between the two."
  (let* ((input (gethash "input" block))
         (description (and (hash-table-p input) (gethash "description" input))))
    (and description
         (one-line (render-value description) *tool-history-value-width*))))

(defun keys-shown-in-body (input)
  "The arguments a call's body lists: all of them but \"description\", which
is already on the headline."
  (and (hash-table-p input)
       (remove "description" (tool-input-keys input) :test #'equal)))

(defun labelled-arg-lines (key input)
  (let ((value (gethash key input)))
    (cons (format nil "~a:" key)
          (tool-value-lines value (tool-arg-language key value input)))))

(defun tool-use-body (input)
  "A call's arguments as body lines. A call whose only argument is a command
is written as the bare command: the headline already names the tool, so a
\"command:\" label above it would be noise."
  (let ((keys (keys-shown-in-body input)))
    (cond
      ((null keys) (fenced (list "(no arguments)")))
      ((equal keys '("command")) (tool-value-lines (gethash "command" input) "bash"))
      (t (loop for key in keys append (labelled-arg-lines key input))))))

(defun record-tool-use (subagent-name block)
  "Write a tool call down: who called what, and what it was handed."
  (append-tool-history
   (tool-history-entry
    (format nil "`~a` ~@[*~a* ~]**~a**~@[ — ~a~]"
            (tool-history-time) subagent-name (gethash "name" block)
            (tool-description block))
    (tool-use-body (gethash "input" block)))))

(defun record-tool-result (subagent-name tool-name description block)
  "Write what a tool answered down as its own entry. Results arrive after
their call and parallel calls interleave, so the headline repeats the tool's
name and DESCRIPTION to say which call came back."
  (let ((lines (history-lines (tool-result-text block) *tool-history-result-width*)))
    (append-tool-history
     (tool-history-entry
      (format nil "`~a` ~@[*~a* ~]**~a** ⤶ ~:[result~;failed~]~@[ — ~a~]"
              (tool-history-time) subagent-name tool-name
              (json-true-p (gethash "is_error" block))
              description)
      (fenced (or lines (list "(no output)")))))))

(defun grey-multiline (string)
  "Like GREY, but with the colour restarted on each line. rlwrap (which
agent-run.sh pipes the REPL through) mishandles a colour reset that lands
past a line boundary, rendering the line in the terminal's default white, so
a span must never straddle a newline."
  (let ((start 0) (len (length string)))
    (with-output-to-string (out)
      (loop
        (let ((newline (position #\Newline string :start start)))
          (write-string (grey (subseq string start (or newline len))) out)
          (unless newline (return))
          (write-char #\Newline out)
          (setf start (1+ newline))
          (when (>= start len) (return)))))))

(defun subagent-spawning-tool-p (name)
  "This CLI build calls it \"Agent\", older and other builds \"Task\"; match
either so subagents do not silently stop being named if it changes back."
  (member name '("Agent" "Task") :test #'equal))

(defun forwarded-subagent-id (event)
  "The tool_use id EVENT was forwarded from, or NIL for the top-level agent's
own messages. shasht parses JSON null as the truthy keyword :NULL, so the
top level's explicit \"parent_tool_use_id\":null has to be excluded by hand."
  (let ((parent (gethash "parent_tool_use_id" event)))
    (and parent (not (eq parent :null)) parent)))

(defun init-event-p (event)
  (and (equal (gethash "type" event) "system")
       (equal (gethash "subtype" event) "init")))

(defun message-event-p (event)
  (member (gethash "type" event) '("assistant" "user") :test #'equal))

(defun message-blocks (event)
  (or (gethash "content" (gethash "message" event)) #()))

(defun narration-block-p (block-type)
  (member block-type '("text" "thinking") :test #'equal))

(defun delta-text (delta)
  "The renderable text a content_block_delta carries, or NIL for a delta of
some other kind. Many thinking deltas arrive genuinely empty."
  (let ((delta-type (gethash "type" delta)))
    (cond
      ((equal delta-type "thinking_delta") (gethash "thinking" delta))
      ((equal delta-type "text_delta") (gethash "text" delta)))))

(defun thinking-delta-p (delta)
  (equal (gethash "type" delta) "thinking_delta"))

(defun text-block-start-p (inner)
  (and (equal (gethash "type" inner) "content_block_start")
       (equal (gethash "type" (gethash "content_block" inner)) "text")))

(defun init-trace-line (event)
  (let ((tools (gethash "tools" event)))
    (format nil "  ⚙ init~@[ · model ~a~]~@[ · cwd ~a~]~@[ · ~a tools~]"
            (gethash "model" event)
            (gethash "cwd" event)
            (and (vectorp tools) (length tools)))))

(defun make-stream-printer ()
  "Return a fresh ON-EVENT callback for CALL-CLAUDE that renders a
stream_event live: grey for thinking, plain for the reply text, and -- with
*VERBOSE* on -- a grey line per tool call and per tool result. Exactly one
blank line separates each transition into a text block, tracked by counting
the trailing newlines already written rather than inserting one blindly,
which double-spaced whenever the model's own text already supplied the gap."
  (let ((trailing-newlines 2) ; RUN's own preamble already ends on a blank line
        (pending-bracket "")
        (subagent-names (make-hash-table :test #'equal))
        (tool-names (make-hash-table :test #'equal))
        (tool-descriptions (make-hash-table :test #'equal))
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
             (restore-sgr-escapes (str)
               "The CLI's text deltas arrive with the ESC of a colour
sequence already eaten, and a bare sequence can itself be split across
chunks (\"[3\" then \"3m\"). Put the ESC back, holding a trailing sequence
that could still complete in PENDING-BRACKET for the next chunk."
               (let* ((full (concatenate 'string pending-bracket str))
                      (len (length full)))
                 (setf pending-bracket "")
                 (with-output-to-string (out)
                   (loop with i = 0
                         while (< i len)
                         do (let ((end (and (char= (char full i) #\[)
                                            (sgr-parameters-end full i))))
                              (cond
                                ((eq end :incomplete)
                                 (setf pending-bracket (subseq full i))
                                 (setf i len))
                                ((integerp end)
                                 (unless (and (plusp i) (char= (char full (1- i)) #\Escape))
                                   (write-char #\Escape out))
                                 (write-string full out :start i :end end)
                                 (setf i end))
                                (t
                                 (write-char (char full i) out)
                                 (incf i))))))))
             (flush-pending! ()
               "A held-back sequence never completes if the block ends right
there -- emit it as literal text rather than losing it."
               (unless (zerop (length pending-bracket))
                 (write-string pending-bracket)
                 (setf pending-bracket "")))
             (print-subagent-block (content-key subagent-name block)
               "A forwarded subagent block arrives whole rather than as
deltas, so it is printed as one unit, tagged with the subagent it came
from. The grey span stops before the newline, per GREY-MULTILINE."
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
               "One grey line of the *VERBOSE* trace. A run of them stays
single spaced -- the common case, a model firing several tools back to back
-- while the first after other output gets a blank line above it. LINE is
newline-free (see ONE-LINE), as GREY-MULTILINE's constraint requires."
               (flush-pending!)
               (if last-line-was-trace (ensure-line-start) (ensure-blank-line))
               (write-string (grey line))
               (write-char #\Newline)
               (track! (concatenate 'string line (string #\Newline)))
               (setf last-line-was-trace t)
               (finish-output))
             (remember-call (block)
               (setf (gethash (gethash "id" block) tool-names) (gethash "name" block)
                     (gethash (gethash "id" block) tool-descriptions) (tool-description block)))
             (print-tool-use (subagent-name block)
               "Record a tool call in the history and, with *VERBOSE* on, echo
it. The history is written either way: *VERBOSE* governs the terminal, not
the file."
               (remember-call block)
               (record-tool-use subagent-name block)
               (when *verbose*
                 (print-trace-line (format nil "  ⚒ ~@[[~a] ~]~a"
                                           subagent-name (format-tool-use block)))))
             (print-tool-result (subagent-name block)
               "Record what a tool answered, and with *VERBOSE* on echo it,
under the name of the call it answers -- a tool_result block itself carries
only the tool_use id."
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
               "Learn a subagent-spawning call's id -> name, so blocks
forwarded under that id are labelled with the subagent they came from
rather than a generic \"subagent\"."
               (when (subagent-spawning-tool-p (gethash "name" block))
                 (let* ((input (gethash "input" block))
                        (name (and (hash-table-p input)
                                   (or (gethash "description" input)
                                       (gethash "subagent_type" input)))))
                   (when name
                     (setf (gethash (gethash "id" block) subagent-names) name))))))
      (labels ((print-delta (delta)
                 (let* ((raw (delta-text delta))
                        (clean (and raw
                                    (strip-terminal-control-chars
                                     (if (thinking-delta-p delta) raw (restore-sgr-escapes raw))))))
                   (when (and clean (plusp (length clean)))
                     (write-string (if (thinking-delta-p delta) (grey-multiline clean) clean))
                     (track! clean)
                     (setf last-line-was-trace nil)
                     (finish-output))))
               (print-stream-event (inner)
                 (let ((inner-type (gethash "type" inner)))
                   (cond
                     ((text-block-start-p inner)
                      (flush-pending!)
                      (ensure-blank-line)
                      (setf last-line-was-trace nil)
                      (finish-output))
                     ((equal inner-type "content_block_stop")
                      (flush-pending!)
                      (finish-output))
                     ((equal inner-type "content_block_delta")
                      (print-delta (gethash "delta" inner))))))
               (subagent-name-of (event)
                 (let ((parent (forwarded-subagent-id event)))
                   (and parent (or (gethash parent subagent-names) "subagent"))))
               (print-message-block (subagent-name block)
                 (let ((block-type (gethash "type" block)))
                   (cond
                     ;; The top-level agent's own narration already streamed
                     ;; in as deltas; only a subagent's arrives whole, here.
                     ((and subagent-name (narration-block-p block-type))
                      (print-subagent-block block-type subagent-name block))
                     ((equal block-type "tool_use")
                      (remember-subagent-name block)
                      (print-tool-use subagent-name block))
                     ((equal block-type "tool_result")
                      (print-tool-result subagent-name block)))))
               (print-message (event)
                 (let ((subagent-name (subagent-name-of event)))
                   (loop for block across (message-blocks event)
                         do (print-message-block subagent-name block)))))
        (lambda (event)
          (cond
            ((equal (gethash "type" event) "stream_event")
             (sb-thread:with-mutex (*output-lock*)
               (print-stream-event (gethash "event" event))))
            ((and *verbose* (init-event-p event))
             (sb-thread:with-mutex (*output-lock*)
               (print-trace-line (init-trace-line event))))
            ((message-event-p event)
             (sb-thread:with-mutex (*output-lock*)
               (print-message event)))))))))

(defun drain-stderr (process)
  "Forward the child's stderr to *error-output*, sanitizing each line, on its
own thread. :error t would instead inherit the terminal directly and let the
CLI's raw progress output bypass sanitization entirely."
  (loop for line = (ignore-errors (read-line (sb-ext:process-error process) nil nil))
        while line
        do (sb-thread:with-mutex (*output-lock*)
             (format *error-output* "[error] ~a~%" (strip-terminal-control-chars line))
             (finish-output *error-output*))))

(defun claude-cli-args (prompt session-id)
  "The CLI invocation for PROMPT. stream-json is the only output format that
reports rate_limit_event lines alongside the result, and partial messages are
what break a reply into deltas we can render as they arrive."
  (append (list "-p" prompt
                "--output-format" "stream-json"
                "--include-partial-messages"
                "--verbose"
                "--model" *model*
                "--permission-mode" *permission-mode*
                ;; Stops the CLI writing its own spinners and borders straight
                ;; to the terminal, bypassing our pipes and our sanitizing.
                "--ax-screen-reader"
                ;; Without it a subagent-heavy turn stays silent until the
                ;; subagent finishes.
                "--forward-subagent-text")
          (when *effort* (list "--effort" *effort*))
          (when session-id (list "--resume" session-id))))

(defun call-claude (prompt on-event)
  "Run the claude CLI on PROMPT, calling ON-EVENT with each parsed JSON event
as it arrives, so the caller can render the turn while it is still working.
Returns (values answer-text session-id total-cost-usd rate-limit-info usage)."
  (let* ((session-id (read-session-id))
         (args (claude-cli-args prompt session-id))
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
;;; Printed just above the separator: where the account stands on the rolling
;;; 5h/7d rate-limit windows, and what the call cost.

(defparameter +reset-time-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
    (:hour 2) #\: (:min 2) #\Space :timezone))

(defun format-reset-time (epoch-seconds)
  "Reset instant in *TIMEZONE*, e.g. \"2026-09-23 03:59 CEST\"."
  (local-time:format-timestring nil (local-time:unix-to-timestamp epoch-seconds)
                                :format +reset-time-format+
                                :timezone (display-timezone)))

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
  "The Session(5h)/Week(7d) lines alone, empty string when RATE-LIMIT is nil."
  (let* ((windows (and rate-limit (gethash "unifiedWindows" rate-limit)))
         (five-hour (format-window "Session (5h)" (and windows (gethash "five_hour" windows))))
         (seven-day (format-window "Week (7d)" (and windows (gethash "seven_day" windows)))))
    (with-output-to-string (s)
      (when five-hour (format s "~a~%" five-hour))
      (when seven-day (format s "~a~%" seven-day)))))

(defun format-tokens (usage)
  "One line of per-call token counts: input and output, plus cache read and
creation when present."
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

;;; Redefined below, once RUN exists for it to point at: this placeholder is
;;; only here so RUN can call USE without a forward reference.
(defun use () nil)

(defun usage ()
  "Print the CLI's own /usage report, plus the exact reset countdown from the
last real call. /usage is answered locally by the CLI rather than by the
model, so it costs nothing and does not touch the conversation history -- and
carries no rate-limit payload of its own, hence *LAST-RATE-LIMIT*."
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
  (remember-agent "AGENT-CLAUDECODE")
  (set-status STATUS-OK)
  *model*)

(defun forget ()
  (forget-mem MEMORY-FILE)
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
  "Turn the verbose tool trace on or off, toggling when called with no
argument. With it on, every tool the CLI runs prints a grey line as it
happens -- name and arguments when it starts, a preview when it answers --
so a long turn shows what it is doing instead of going quiet."
  (setf *verbose* (and on t))
  (format t "~&Verbose: ~:[off~;on~]~%" *verbose*)
  *verbose*)

(defun verbose (&optional (on (not *verbose*)))
  "Shorthand for SET-VERBOSE, to type at the REPL."
  (set-verbose on))
