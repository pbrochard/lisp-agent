(defpackage :common
  (:use :cl :utils)
  (:export #:SYSTEM-PROMPT #:*CURRENT-RUN-FN* #:*current-model* #:*MEMORY-FILE* #:*SYSTEM-MESSAGE* #:SEP #:OBJ #:LISP-EVAL #:RECALL #:REMEMBER #:FORGET-MEM #:BASH #:SET-STATUS #:STATUS-THINKING #:STATUS-OK)
  (:nicknames :c :co))

(in-package :common)

(defconstant SYSTEM-PROMPT "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions. You live in a Docker container without sudo or root access. Ask if you need a software to perform a task.")

(defparameter *current-run-fn* nil)
(defparameter *current-model* "")

(defconstant SEP "___________________________________________________________________________")
(defconstant STATUS-THINKING " thinking...")
(defconstant STATUS-OK "")

;;; --- tiny JSON helpers -------------------------------------------------
;;; shasht reads JSON objects as hash tables; OBJ builds them going out.

(defun obj (&rest kvs)
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on kvs by #'cddr
        do (setf (gethash k h) v)
        finally (return h)))

;;; --- the tool: a Lisp REPL ---------------------------------------------
(defun lisp-eval (form-string)
  "The agent's hands. Read a form, eval it, print what came back."
  (handler-case
      (format nil "~s" (eval (read-from-string form-string)))
    (error (e) (format nil "ERROR: ~a" e))))

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

(defun forget-all ()
  "Finds all /agent/data/memory-*.json files and deletes them from the filesystem."
  (let ((directory "/agent/data/")
        (pattern "memory-")
        (extension ".json"))
    ;; Find all files matching the pattern
    (let ((files (directory (merge-pathnames (format nil "~a~a*~a" directory pattern extension) directory))))
      (if (null files)
          (format t "No matching memory files found.~%")
          (progn
            (dolist (file files)
              (delete-file file)
              (format t "Deleted: ~a~%" file))
			(format t "Successfully deleted ~a file(s).~%" (length files)))))))

;;; Shell helper
(defun bash ()
  (sb-ext:run-program "/bin/bash" nil :output t :input t :search t))

;;; Generic run
(defun run (prompt)
  (funcall *current-run-fn* prompt))

(defun memo ()
  (run "Write down in the ./data/knowledge.md file what you have learned so far to share it with other IA"))

(defun learn ()
  (run "Learn what you should know so far from the file ./data/knowledge.md"))

;; Status functions
(defun set-status (status)
  (with-open-file (out "./data/status" :direction :output :if-exists :supersede)
	(format out "[AI:~a~a]" *current-model* status)))
