(defpackage :common
  (:use :cl)
  (:export #:SYSTEM-PROMPT #:*CURRENT-RUN-FN* #:*MEMORY-FILE* #:SEP #:OBJ #:REF #:LISP-EVAL #:RECALL #:REMEMBER #:FORGET #:BASH)
  (:nicknames :c :co))

(in-package :common)

(defconstant SYSTEM-PROMPT "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions. You live in a Docker container without sudo or root access. Ask if you need a software to perform a task.")

(defparameter *current-run-fn* nil)

(defconstant SEP "___________________________________________________________________________")

;;; --- tiny JSON helpers -------------------------------------------------
;;; shasht reads JSON objects as hash tables; OBJ builds them going out.

(defun obj (&rest kvs)
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on kvs by #'cddr
        do (setf (gethash k h) v)
        finally (return h)))

(defun ref (table &rest keys)
  "Walk nested hash tables / vectors: (ref x \"choices\" 0 \"message\")"
  (reduce (lambda (acc key)
            (etypecase key
              (string (gethash key acc))
              (integer (aref acc key))))
          keys :initial-value table))

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

(defparameter *system-message*
  (obj "role" "system"
       "content" SYSTEM-PROMPT))

(defun remember (messages)
  (with-open-file (out *memory-file* :direction :output :if-exists :supersede)
    (shasht:write-json (coerce messages 'vector) out))
  messages)

(defun recall ()
  (if (probe-file *memory-file*)
      (coerce (with-open-file (in *memory-file*) (shasht:read-json in)) 'list)
      (list *system-message*)))

(defun forget ()
  (when (probe-file *memory-file*) (delete-file *memory-file*))
  (format t "~&Memory wiped.~%~a~%" SEP))

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
