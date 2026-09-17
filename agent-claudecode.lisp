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
           #:set-effort #:list-efforts #:le #:*efforts* #:usage)
  (:nicknames :cc :claudecode :ccode))

(in-package :agent-claudecode)

(defparameter *claude-bin* "claude")
(defparameter *model* "sonnet")
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

(defconstant MEMORY-FILE "/agent/data/memory-claudecode.json")
(defconstant SESSION-FILE "/agent/data/session-claudecode.txt")

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

(defun strip-terminal-control-chars (string)
  "Strip characters that could rewrite, erase, or visually spoof
already-printed terminal output (see UNSAFE-TERMINAL-CHAR-P). Model output
is untrusted and must not be allowed to manipulate the terminal it's
printed to. Returns STRING unchanged (no copy) when nothing needs
stripping, since that is the overwhelmingly common case on the streaming
hot path."
  (if (find-if #'unsafe-terminal-char-p string)
      (remove-if #'unsafe-terminal-char-p string)
      string))

(defun make-stream-printer ()
  "Return a fresh ON-EVENT callback for CALL-CLAUDE that renders a
stream_event live: dim grey for thinking, plain for the reply text. Ensures
exactly one blank line separates each transition into a text block —
thinking -> answering, but also text -> tool call -> text when the model
keeps talking after using a tool — regardless of how many newlines the
model's own content happens to carry across that boundary. Tracked via the
count of trailing newlines already written (capped at 2) rather than an
unconditional insert, since always inserting one (the previous approach)
double-spaced whenever the model's own text already supplied the gap, and
inserting only once ever under-spaced every later transition."
  (let ((trailing-newlines 2)) ; RUN's own preamble already ends on a blank line
    (labels ((track! (str)
               (loop for ch across str
                     do (setf trailing-newlines (if (char= ch #\Newline) (min 2 (1+ trailing-newlines)) 0))))
             (ensure-blank-line ()
               (loop while (< trailing-newlines 2)
                     do (write-char #\Newline)
                        (incf trailing-newlines))))
      (lambda (event)
        (when (equal (gethash "type" event) "stream_event")
          (sb-thread:with-mutex (*output-lock*)
            (let* ((inner (gethash "event" event))
                   (inner-type (gethash "type" inner)))
              (cond
                ((and (equal inner-type "content_block_start")
                      (equal (gethash "type" (gethash "content_block" inner)) "text"))
                 (ensure-blank-line))
                ((equal inner-type "content_block_delta")
                 (let* ((delta (gethash "delta" inner))
                        (delta-type (gethash "type" delta)))
                   (cond
                     ((equal delta-type "thinking_delta")
                      (let ((clean (strip-terminal-control-chars (gethash "thinking" delta))))
                        (write-string (grey clean))
                        (track! clean)))
                     ((equal delta-type "text_delta")
                      (let ((clean (strip-terminal-control-chars (gethash "text" delta))))
                        (write-string clean)
                        (track! clean)))))))
              (finish-output))))))))

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
                              "--permission-mode" *permission-mode*)
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
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (+ epoch-seconds +UNIX-EPOCH-UNIVERSAL-TIME+) 0)
    (declare (ignore sec))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC" year month date hour min)))

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
