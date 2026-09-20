(defpackage :common
  (:use :cl :utils :cl-ansi-text :uiop)
  (:export #:SYSTEM-PROMPT #:*CURRENT-RUN-FN* #:*current-model* #:*MEMORY-FILE* #:*SYSTEM-MESSAGE* #:SEP #:GREY #:RECALL
		   #:REMEMBER #:FORGET-MEM #:FORGET-ALL #:BASH #:CD #:SET-STATUS #:STATUS-THINKING #:STATUS-OK
		   #:GET-PROMPT #:EP #:RP #:P #:ENP #:NP #:R)
  (:nicknames :c :co))

(in-package :common)

(defconstant SYSTEM-PROMPT "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions. You live in a Docker container without sudo or root access. Ask if you need a software to perform a task.")

(defparameter *current-run-fn* nil)
(defparameter *current-model* "")

(defparameter *editor* "/usr/bin/vi")
(defconstant PROMP_PATH "/agent/data/prompt.md")

(defconstant SEP (green  "___________________________________________________________________________"))
(defconstant STATUS-THINKING " thinking...")
(defconstant STATUS-OK "")

(defmacro defalias (alias original)
  "Make ALIAS share ORIGINAL's function object, so calling ALIAS doesn't add an extra funcall."
  `(setf (fdefinition ',alias) (fdefinition ',original)))

(defun grey (string)
  "cl-ansi-text only ships the 8 basic ANSI colors, none of them grey, so this
needs a wider palette. :24bit (truecolor, 38;2;r;g;b) looks right in a
directly-attached terminal but many terminal emulators/multiplexers -- tmux
or screen without an explicit Tc/RGB override, older terminal apps, some
SSH/web terminals -- don't understand it and silently render the default
foreground color instead of grey. :8bit (the 256-color palette, 38;5;n) has
been near-universally supported since the late 90s, so it renders as grey
almost everywhere truecolor might silently fail."
  (let ((*color-mode* :8bit))
    (with-output-to-string (s)
      (with-color ("#808080" :stream s)
        (write-string string s)))))

;;; --- memory ---------------------------------------------------------------
;;; Messages are already a list of hash tables, i.e. already JSON.
;;; So memory is nothing more than writing that list down and reading it back.

(defparameter *memory-file*
  (pathname (or (uiop:getenv "AGENT_MEMORY") "/agent/data/memory.json")))

(defparameter *system-message* '())

(defun remember (messages)
  (with-open-file (out *memory-file* :direction :output :if-exists :supersede)
    (shasht:write-json (coerce messages 'vector) out))
  messages)

(defun recall ()
  (if (probe-file *memory-file*)
      (coerce (with-open-file (in *memory-file*) (shasht:read-json in)) 'list)
      *system-message*))

(defun forget-mem ()
  (when (probe-file *memory-file*) (delete-file *memory-file*))
  (format t "~&Memory wiped: ~a.~%~a~%" *memory-file* SEP))

;; Package names, not literal SYMBOL-QUALIFIED::NAMES: common.lisp loads
;; before any agent package exists, so the reader would choke on a
;; package-qualified symbol at load time. FIND-PACKAGE/FIND-SYMBOL resolve
;; by name at call time instead, once everything is actually loaded.
(defparameter *agent-packages* '("AGENT-CLAUDE" "AGENT-GEMINI" "AGENT-OLLAMA" "AGENT-CLAUDECODE" "AGENT-DEEPSEEK"))

(defun forget-all ()
  "Calls FORGET in every agent package instead of duplicating each one's own
cleanup by deleting /agent/data/memory-*.json files directly: that missed
whatever extra state a given agent also needs to clear, e.g.
agent-claudecode's own session file alongside its memory."
  (dolist (package-name *agent-packages*)
    (let* ((package (find-package package-name))
           (fn (and package (find-symbol "FORGET" package))))
      (when (and fn (fboundp fn))
        (funcall fn)))))

;;; Shell helper
(defun bash ()
  (sb-ext:run-program "/bin/bash" nil :output t :input t :search t))

(defun cd (&optional path)
  (when path
	(chdir path))
  (getcwd))

;;; Generic run
(defun run (prompt)
  (funcall *current-run-fn* prompt))

(defun memo ()
  (run "Write down in the ./data/knowledge.md file what you have learned so far to share it with other IA. Acknowledge and output nothing else."))

(defun learn-from-knowledge ()
  (run "Learn what you should know so far from the file ./data/knowledge.md. Acknowledge and output nothing else."))

(defun learn-from-skill (skill)
  (let ((skill-str (string-downcase skill)))
	(run (format nil "Learn what you should know on skill `~a` from the file ./skills/~a/SKILL.md. Acknowledge and output nothing else." skill-str skill-str))))

(defun learn (&optional skill)
  (if (not skill)
	  (learn-from-knowledge)
	  (learn-from-skill skill)))

;; Status functions
(defun set-status (status)
  (with-open-file (out "./data/status" :direction :output :if-exists :supersede)
	(format out "[AI:~a~a]" *current-model* status)))

;; Prompt helpers
(defun get-prompt ()
  (let ((prompt ""))
	(when (probe-file PROMP_PATH)
	  (with-open-file (in PROMP_PATH :direction :input)
		(loop for line = (read-line in nil nil)
			  while line
			  do (setf prompt (format nil "~a~&~a" prompt line)))))
	prompt))

(defun print-prompt (prompt)
  (format t "~a~%" (yellow prompt)))

(defconstant CANCEL-MARKER "!no!")

;; Lets a prompt written to the prompt file veto its own run: typing !no!
;; anywhere in it (e.g. after editing and changing your mind) skips RUN
;; instead of deleting the whole prompt.
(defun run-unless-cancelled (prompt)
  (if (search CANCEL-MARKER prompt)
	  (format t "~&Cancelled: prompt contains `~a`.~%~a~%" CANCEL-MARKER SEP)
	  (run prompt)))

;; Edit prompt
(defun edit-prompt ()
  (sb-ext:run-program *editor* `(,PROMP_PATH) :output t :input t :search t)
  (print-prompt (get-prompt)))

(defalias ep edit-prompt)

;; Edit new prompt
(defun edit-new-prompt ()
  (when (probe-file PROMP_PATH)
	(delete-file PROMP_PATH))
  (edit-prompt))

(defalias enp edit-new-prompt)

;; Run prompt
(defun run-prompt ()
  (let ((prompt (get-prompt)))
	(print-prompt prompt)
	(run-unless-cancelled prompt)))

(defalias rp run-prompt)

;; Edit and run prompt
(defun edit-run-prompt ()
  (edit-prompt)
  (run-unless-cancelled (get-prompt)))

(defalias p edit-run-prompt)

;; Edit and run new prompt
(defun edit-run-new-prompt ()
  (edit-new-prompt)
  (run-unless-cancelled (get-prompt)))

(defalias np edit-run-new-prompt)
(defalias r edit-run-new-prompt)

