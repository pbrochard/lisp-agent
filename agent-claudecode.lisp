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
  (:export #:run #:use #:forget #:set-model)
  (:nicknames :cc :claudecode :ccode))

(in-package :agent-claudecode)

(defparameter *claude-bin* "claude")
(defparameter *model* "sonnet")
(defparameter *permission-mode* "bypassPermissions")

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

(defun print-stream-delta (event)
  "Render a stream_event EVENT live: dim grey for thinking, plain for the
reply text, with a blank line when the model switches from thinking to
answering."
  (when (equal (gethash "type" event) "stream_event")
    (let* ((inner (gethash "event" event))
           (inner-type (gethash "type" inner)))
      (cond
        ((and (equal inner-type "content_block_start")
              (equal (gethash "type" (gethash "content_block" inner)) "text"))
         (format t "~&~%"))
        ((equal inner-type "content_block_delta")
         (let* ((delta (gethash "delta" inner))
                (delta-type (gethash "type" delta)))
           (cond
             ((equal delta-type "thinking_delta")
              (write-string (grey (gethash "thinking" delta))))
             ((equal delta-type "text_delta")
              (write-string (gethash "text" delta)))))))
      (finish-output))))

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
                       (when session-id (list "--resume" session-id))))
         (process (sb-ext:run-program *claude-bin* args
                                       :output :stream :error t
                                       :wait nil :search t))
         (result nil)
         (rate-limit nil))
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

(defun format-window (label window)
  (when window
    (let ((utilization (gethash "utilization" window))
          (resets-at (gethash "resetsAt" window)))
      (format nil "~a: ~,1f% used - resets ~a"
              label (* 100 utilization)
              (format-reset-time resets-at)))))

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
  (let* ((windows (and rate-limit (gethash "unifiedWindows" rate-limit)))
         (five-hour (format-window "Session (5h)" (and windows (gethash "five_hour" windows))))
         (seven-day (format-window "Week (7d)" (and windows (gethash "seven_day" windows))))
         (tokens (format-tokens usage)))
    (with-output-to-string (s)
      (when five-hour (format s "~a~%" five-hour))
      (when seven-day (format s "~a~%" seven-day))
      (when tokens (format s "~a~%" tokens))
      (format s "Cost: $~,4f this session" (or cost 0)))))

;;; --- entry point ------------------------------------------------------------

(defun use () nil)

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (format t "~&______~&~%")
  (multiple-value-bind (text session-id cost rate-limit usage)
      (call-claude prompt #'print-stream-delta)
    (when session-id (write-session-id session-id))
    (remember (append (recall)
                      (list (obj "role" "user" "content" prompt)
                            (obj "role" "assistant" "content" text))))
    (format t "~&~%~a~%~a ~a~%"
			(grey (format-usage cost rate-limit usage))
			SEP (grey *model*))
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

(defun set-model (name)
  (setf *model* name)
  (use))
