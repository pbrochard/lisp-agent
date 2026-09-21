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
  (:use :cl :utils :http-utils :common)
  (:export #:run #:use #:forget #:usage)
  (:nicknames :a :ag))

(in-package :agent)

(defparameter *endpoint* "https://openrouter.ai/api/v1/chat/completions")
;;(defparameter *model* "anthropic/claude-sonnet-4.5")
(defparameter *model* "google/gemma-4-31B-it")
(defparameter *api-key* (uiop:getenv "API_KEY_OPENROUTER"))

(defconstant MEMORY-FILE "/agent/data/memory-agent.json")

(defparameter *last-usage* nil
  "The token usage the last model call reported, kept so USAGE can show
what a turn cost. NIL until a call has been made.")

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
    (format t "~&  ⤷ ~a~&    => ~a~%" (grey (gethash "form" args)) (grey result))
    (obj "role" "tool"
         "tool_call_id" (gethash "id" tool-call)
         "content" result)))

;;; --- talking to the model ----------------------------------------------

(defun call-model (messages)
  (http-post-json
   *endpoint*
   `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
     ("Content-Type" . "application/json"))
   (obj "model" *model*
        "messages" (coerce messages 'vector)
        "tools" *tools*)))

;;; --- the loop itself ----------------------------------------------------
;;; An agent is a recursive function over a growing list of messages.
;;; Base case: the model answers in words. Recursive case: it asks
;;; for tools, we run them, and recur with the enriched history.

(defun agent-loop (messages)
  "Returns the complete message history, final answer included.
The answer is just (gethash \"content\" (car (last messages)))."
  (let* ((response (call-model messages))
         (message (ref response "choices" 0 "message"))
         (tool-calls (gethash "tool_calls" message)))
    (setf *last-usage* (gethash "usage" response))
    (if (and tool-calls (plusp (length tool-calls)))
        (agent-loop (append messages
                            (list message)
                            (map 'list #'execute tool-calls)))
        (append messages (list message)))))

;;; --- entry point ------------------------------------------------------------
(defun use () nil)

;;; --- usage & token reporting ------------------------------------------
;;; OpenRouter answers in the OpenAI-compatible shape, so a call's token
;;; counts come back in the response's USAGE object. The account balance
;;; lives with whichever provider OpenRouter routed to and is not exposed
;;; here, so this reports what a call cost, not what is left.

(defun usage (&optional date)
  "Report usage. The last model call's token counts come first, when there was
one -- what that turn cost. OpenRouter exposes no account balance through
this API, so there is nothing to add the way DeepSeek and OpenAI allow.
DATE is accepted so (usage) is uniform across agents and ignored."
  (declare (ignore date))
  (openai-utils:print-usage-tokens *last-usage*))

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user" "content" prompt)))))))
    (format t "~&______~&~%~a~%" (gethash "content" (car (last history))))
    (let ((tokens (openai-utils:format-usage-tokens *last-usage*)))
      (when tokens (format t "~&~a~%" (grey tokens))))
    (format t "~&~a ~a:~a~%" SEP (grey (recorded-agent-name)) (grey *model*))
	(set-status STATUS-OK)))

(defun use ()
  (setf *current-run-fn* #'run
		*current-model* *model*
		*memory-file* (pathname MEMORY-FILE)
		*system-message* (list (obj "role" "system"
									"content" SYSTEM-PROMPT)))
  (remember-agent "AGENT")
  (set-status STATUS-OK)
  *model*)

(defun forget ()
  (forget-mem MEMORY-FILE))
