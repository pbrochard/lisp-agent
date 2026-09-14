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
  (:use :cl :utils :common)
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

(defun call-claude (prompt)
  "Run the claude CLI on PROMPT. Returns (values answer-text session-id)."
  (let* ((session-id (read-session-id))
         (args (append (list "-p" prompt
                              "--output-format" "json"
                              "--model" *model*
                              "--permission-mode" *permission-mode*)
                       (when session-id (list "--resume" session-id))))
         (output (with-output-to-string (out)
                   (sb-ext:run-program *claude-bin* args
                                        :output out :error out :search t)))
         (json (shasht:read-json output)))
    (values (gethash "result" json) (gethash "session_id" json))))

;;; --- entry point ------------------------------------------------------------

(defun use () nil)

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (multiple-value-bind (text session-id) (call-claude prompt)
    (when session-id (write-session-id session-id))
    (remember (append (recall)
                       (list (obj "role" "user" "content" prompt)
                             (obj "role" "assistant" "content" text))))
    (format t "~&______~&~%~a~%~a ~a~%" text SEP *model*)
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
