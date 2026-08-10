;;;; agent.lisp — a recursive agent loop in Common Lisp
;;;;
;;;; The agent's only tool is EVAL. Homoiconicity does the rest:
;;;; the model writes Lisp, the loop runs it, the result flows back.
;;;;
;;;; Usage:
;;;;   ollama pull qwen3:8b     ; and make sure the ollama server is running
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
  (:export #:run #:forget))

(in-package :agent)

(defparameter *endpoint* "http://localhost:11434/api/chat")
(defparameter *model* "qwen3:8b")
(defparameter *system-prompt*
  "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions.")

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

(defparameter *tools*
  (vector
   (obj "type" "function"
        "function"
        (obj "name" "lisp-eval"
             "description" "Evaluate a Common Lisp form and return the printed result. Use this for computation, list manipulation, anything."
             "parameters"
             (obj "type" "object"
                  "properties" (obj "form" (obj "type" "string"
                                                "description" "A single Common Lisp form, e.g. (reduce #'+ (loop for i from 1 to 100 collect i))"))
                  "required" (vector "form"))))))

(defun lisp-eval (form-string)
  "The agent's hands. Read a form, eval it, print what came back."
  (handler-case
      (format nil "~s" (eval (read-from-string form-string)))
    (error (e) (format nil "ERROR: ~a" e))))

(defun execute (tool-call)
  "Turn one tool call from the model into a tool-result message."
  (let* ((fn (gethash "function" tool-call))
         (name (gethash "name" fn))
         (args (gethash "arguments" fn))
         (result (if (string= name "lisp-eval")
                     (lisp-eval (gethash "form" args))
                     (format nil "ERROR: unknown tool ~a" name))))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "role" "tool"
         "tool_name" name
         "content" result)))

;;; --- talking to the model ----------------------------------------------

(defun call-model (messages)
  (shasht:read-json
   (dex:post *endpoint*
             :headers '(("content-type" . "application/json"))
             :content (shasht:write-json
                       (obj "model" *model*
                            "stream" :false
                            "messages"
                            (coerce (cons (obj "role" "system" "content" *system-prompt*)
                                          messages)
                                    'vector)
                            "tools" *tools*)
                       nil))))

;;; --- the loop itself ----------------------------------------------------
;;; An agent is a recursive function over a growing list of messages.
;;; Base case: the model answers in words. Recursive case: it asks
;;; for tools, we run them, and recur with the enriched history.

(defun agent-loop (messages)
  "Returns the complete message history, final answer included."
  (let* ((message (gethash "message" (call-model messages)))
         (tool-calls (gethash "tool_calls" message)))
    (if (and tool-calls (plusp (length tool-calls)))
        (agent-loop (append messages
                            (list message)
                            (map 'list #'execute tool-calls)))
        (append messages (list message)))))

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
  (format t "~&Memory wiped.~%"))

;;; --- entry point ------------------------------------------------------------

(defun final-text (message)
  "The assistant's text answer."
  (gethash "content" message))

(defun run (prompt)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user" "content" prompt)))))))
    (format t "~&~a~%" (final-text (car (last history))))))
