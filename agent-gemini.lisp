;;;; agent.lisp — a recursive agent loop in Common Lisp
;;;;
;;;; The agent's only tool is EVAL. Homoiconicity does the rest:
;;;; the model writes Lisp, the loop runs it, the result flows back.
;;;;
;;;; Usage:
;;;;   export API_KEY=...
;;;;   sbcl --load agent.lisp --eval '(agent:run "What is the 30th Fibonacci number? Compute it.")'
;;;;
;;;; Memory: the full conversation persists to memory.json between runs.
;;;;   (agent:run "My name is Jamie.")
;;;;   ...later, in a fresh process...
;;;;   (agent:run "What is my name?")   ; => it remembers
;;;;   (agent:forget)                   ; wipe the slate

(ql:quickload '(:dexador :shasht) :silent t)

(defpackage :agent
  (:use :cl)
  (:export #:run #:forget #:bash))

(in-package :agent)

(defparameter *endpoint* "https://generativelanguage.googleapis.com/v1beta/models/")
(defparameter *model* "gemini-3.5-flash")
;;(defparameter *model* "gemini-3.1-pro-preview")
(defparameter *api-key* (uiop:getenv "API_KEY"))
(defparameter *system-prompt*
  "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions. You live in a Docker container without sudo or root access. Ask if you need a software to perform a task.")

(defconstant SEP "___________________________________________________________________________")

;;; --- tiny JSON helpers -------------------------------------------------
;;; shasht reads JSON objects as hash tables; OBJ builds them going out.

(defun obj (&rest kvs)
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on kvs by #'cddr
        do (setf (gethash k h) v)
        finally (return h)))

(defun ref (table &rest keys)
  "Walk nested hash tables / vectors: (ref x \"candidates\" 0 \"content\")"
  (reduce (lambda (acc key)
            (etypecase key
              (string (gethash key acc))
              (integer (aref acc key))))
          keys :initial-value table))

;;; --- the tool: a Lisp REPL ---------------------------------------------

(defparameter *tools*
  (vector
   (obj "function_declarations"
        (vector
         (obj "name" "lisp-eval"
              "description" "Evaluate a Common Lisp form and return the printed result. Use this for computation, list manipulation, anything."
              "parameters"
              (obj "type" "object"
                   "properties" (obj "form" (obj "type" "string"
                                                 "description" "A single Common Lisp form, e.g. (reduce #'+ (loop for i from 1 to 100 collect i))"))
                   "required" (vector "form")))))))

(defun lisp-eval (form-string)
  "The agent's hands. Read a form, eval it, print what came back."
  (handler-case
      (format nil "~s" (eval (read-from-string form-string)))
    (error (e) (format nil "ERROR: ~a" e))))

(defun execute (fn-call)
  "Turn one functionCall from the model into a functionResponse part."
  (let* ((name (gethash "name" fn-call))
         (args (gethash "args" fn-call))
         (id (gethash "id" fn-call))
         (result (if (string= name "lisp-eval")
                     (lisp-eval (gethash "form" args))
                     (format nil "ERROR: unknown tool ~a" name)))
         (fr (obj "name" name "response" (obj "result" result))))
    (when id (setf (gethash "id" fr) id))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "functionResponse" fr)))

;;; --- talking to the model ----------------------------------------------

(defun call-model (contents)
  (shasht:read-json
   (dex:post (format nil "~a~a:generateContent" *endpoint* *model*)
             :headers `(("x-goog-api-key" . ,*api-key*)
                        ("content-type" . "application/json"))
             :content (shasht:write-json
                       (obj "system_instruction"
                            (obj "parts" (vector (obj "text" *system-prompt*)))
                            "contents" (coerce contents 'vector)
                            "tools" *tools*)
                       nil))))

;;; --- the loop itself ----------------------------------------------------
;;; An agent is a recursive function over a growing list of messages.
;;; Base case: the model answers in words. Recursive case: it asks
;;; for tools, we run them, and recur with the enriched history.

(defun agent-loop (contents)
  "Returns the complete contents history, final answer included."
  (let* ((message (ref (call-model contents) "candidates" 0 "content"))
         (calls (loop for p across (gethash "parts" message)
                      for fc = (gethash "functionCall" p)
                      when fc collect fc)))
    (if calls
        (agent-loop (append contents
                            (list message)
                            (list (obj "role" "user"
                                       "parts" (map 'vector #'execute calls)))))
        (append contents (list message)))))

;;; --- memory ---------------------------------------------------------------
;;; Messages are already a list of hash tables, i.e. already JSON.
;;; So memory is nothing more than writing that list down and reading it back.

(defparameter *memory-file*
  (pathname (or (uiop:getenv "AGENT_MEMORY") "memory.json")))

(defun remember (messages)
  (with-open-file (out *memory-file* :direction :output :if-exists :supersede)
    (shasht:write-json (coerce messages 'vector) out))
  messages)

(defun recall ()
  (if (probe-file *memory-file*)
      (coerce (with-open-file (in *memory-file*) (shasht:read-json in)) 'list)
      '()))

(defun forget ()
  (when (probe-file *memory-file*) (delete-file *memory-file*))
  (format t "~&Memory wiped.~%~a~%" SEP))

;;; --- entry point ------------------------------------------------------------

(defun final-text (message)
  "Concatenate the text parts of a model message."
  (with-output-to-string (s)
    (loop for p across (gethash "parts" message)
          for text = (gethash "text" p)
          when (and text (not (gethash "thought" p)))
            do (write-string text s))))

(defun run (prompt)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user"
                                      "parts" (vector (obj "text" prompt)))))))))
    (format t "~&-----~&~a~%~a~%" (final-text (car (last history))) SEP)))

(defun bash ()
  (sb-ext:run-program "/bin/bash" nil :output t :input t :search t))
