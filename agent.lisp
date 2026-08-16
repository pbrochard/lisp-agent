;;;; agent.lisp — a recursive agent loop in Common Lisp
;;;;
;;;; The agent's only tool is EVAL. Homoiconicity does the rest:
;;;; the model writes Lisp, the loop runs it, the result flows back.
;;;;
;;;; Usage:
;;;;   export OPENROUTER_API_KEY=sk-or-...
;;;;   sbcl --load agent.lisp --eval '(agent:run "What is the 30th Fibonacci number? Compute it.")'
;;;;
;;;; Memory: the full conversation persists to memory.json between runs.
;;;;   (agent:run "My name is Jamie.")
;;;;   ...later, in a fresh process...
;;;;   (agent:run "What is my name?")   ; => it remembers
;;;;   (agent:forget)                   ; wipe the slate

(defpackage :agent
  (:use :cl :utils :common)
  (:export #:run #:use #:forget)
  (:nicknames :a :ag))

(in-package :agent)

(defparameter *endpoint* "https://openrouter.ai/api/v1/chat/completions")
;;(defparameter *model* "anthropic/claude-sonnet-4.5")
(defparameter *model* "google/gemma-4-31B-it")
(defparameter *api-key* (uiop:getenv "API_KEY_OPENROUTER"))

(defconstant MEMORY-FILE "/agent/data/memory-agent.json")

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

(defun execute (tool-call)
  "Turn one tool-call from the model into a tool-result message."
  (let* ((name (ref tool-call "function" "name"))
         (args (shasht:read-json (ref tool-call "function" "arguments")))
         (result (if (string= name "lisp-eval")
                     (lisp-eval (gethash "form" args))
                     (format nil "ERROR: unknown tool ~a" name))))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "role" "tool"
         "tool_call_id" (gethash "id" tool-call)
         "content" result)))

;;; --- talking to the model ----------------------------------------------

(defun call-model (messages)
  (shasht:read-json
   (dex:post *endpoint*
             :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                        ("Content-Type" . "application/json"))
             :content (shasht:write-json
                       (obj "model" *model*
                            "messages" (coerce messages 'vector)
                            "tools" *tools*)
                       nil))))

;;; --- the loop itself ----------------------------------------------------
;;; An agent is a recursive function over a growing list of messages.
;;; Base case: the model answers in words. Recursive case: it asks
;;; for tools, we run them, and recur with the enriched history.

(defun agent-loop (messages)
  "Returns the complete message history, final answer included.
The answer is just (gethash \"content\" (car (last messages)))."
  (let* ((message (ref (call-model messages) "choices" 0 "message"))
         (tool-calls (gethash "tool_calls" message)))
    (if (and tool-calls (plusp (length tool-calls)))
        (agent-loop (append messages
                            (list message)
                            (map 'list #'execute tool-calls)))
        (append messages (list message)))))

;;; --- entry point ------------------------------------------------------------
(defun use () nil)

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user" "content" prompt)))))))
    (format t "~&______~&~%~a~%~a ~a~%" (gethash "content" (car (last history))) SEP *model*)
	(set-status STATUS-OK)))

(defun use ()
  (setf *current-run-fn* #'run
		*current-model* *model*
		*memory-file* (pathname MEMORY-FILE)
		*system-message* (list (obj "role" "system"
									"content" SYSTEM-PROMPT)))
  (set-status STATUS-OK)
  *model*)

(defun forget ()
  (let ((*memory-file* (pathname MEMORY-FILE)))
	(forget-mem)))
