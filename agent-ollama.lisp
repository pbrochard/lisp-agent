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

(defpackage :agent-ollama
  (:use :cl utils :http-utils :common)
  (:export #:run #:use #:forget)
  (:nicknames :ol :ollama))

(in-package :agent-ollama)

(defparameter *endpoint* "http://localhost:11434/api/chat")
;;(defparameter *model* "qwen3:8b")
;;(defparameter *model* "deepseek-r1:latest")
(defparameter *model* "qwen3")

(defconstant MEMORY-FILE "/agent/data/memory-ollama.json")

;;; --- the tool: a Lisp REPL ---------------------------------------------

(defparameter *tools*
  (vector
   (obj "type" "function"
        "function"
        (obj "name" (lisp-eval-tool-name)
             "description" (lisp-eval-tool-description)
             "parameters" (lisp-eval-tool-parameters)))))

(defun execute (tool-call)
  "Turn one tool call from the model into a tool-result message."
  (let* ((fn (gethash "function" tool-call))
         (name (gethash "name" fn))
         (args (gethash "arguments" fn))
         (result (run-lisp-eval-tool name args)))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "role" "tool"
         "tool_name" name
         "content" result)))

;;; --- talking to the model ----------------------------------------------

(defun call-model (messages)
  (http-post-json
   *endpoint*
   '(("content-type" . "application/json"))
   (obj "model" *model*
        "stream" :false
        "messages" (coerce (cons (obj "role" "system" "content" system-prompt)
                                 messages)
                           'vector)
        "tools" *tools*)
   :read-timeout 100000))

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

;;; --- entry point ------------------------------------------------------------

(defun final-text (message)
  "The assistant's text answer."
  (gethash "content" message))

(defun use () nil)

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user" "content" prompt)))))))
    (format t "~&______~&~%~a~%~a ~a~%" (final-text (car (last history))) SEP (grey *model*))
	(set-status STATUS-OK)))

(defun use ()
  (setf *current-run-fn* #'run
		*current-model* *model*
		*memory-file* (pathname MEMORY-FILE)
		*system-message* '())
  (remember-agent "AGENT-OLLAMA")
  (set-status STATUS-OK)
  *model*)

(defun forget ()
  (forget-mem MEMORY-FILE))
