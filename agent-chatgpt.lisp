;;;; agent.lisp — a recursive agent loop in Common Lisp
;;;;
;;;; The agent's only tool is EVAL. Homoiconicity does the rest:
;;;; the model writes Lisp, the loop runs it, the result flows back.
;;;;
;;;; Usage:
;;;;   export API_KEY_OPENAI=sk-...
;;;;   sbcl --load load.lisp --eval '(chatgpt:run "What is the 30th Fibonacci number? Compute it.")'
;;;;
;;;; Memory: the full conversation persists to memory.json between runs.
;;;;   (chatgpt:run "My name is Jamie.")
;;;;   ...later, in a fresh process...
;;;;   (chatgpt:run "What is my name?")   ; => it remembers
;;;;   (chatgpt:forget)                   ; wipe the slate

(defpackage :agent-chatgpt
  (:use :cl :utils :http-utils :openai-utils :common)
  (:export #:run #:use #:forget #:list-models #:*models* #:lm #:llm #:set-model #:usage)
  (:nicknames :gpt :chatgpt :oai))

(in-package :agent-chatgpt)

(defparameter *endpoint* "https://api.openai.com/v1/chat/completions")
(defparameter *model* "gpt-5.5")
(defparameter *api-key* (uiop:getenv "API_KEY_OPENAI"))

(defparameter *models* nil)

(defparameter *last-usage* nil
  "The token usage the last model call reported, kept so USAGE can show
what a turn cost. NIL until a call is made.")

(defconstant MEMORY-FILE "/agent/data/memory-chatgpt.json")

;;; --- the tool: a Lisp REPL ---------------------------------------------

(defparameter *tools*
  (vector
   (obj "type" "function"
        "function"
        (obj "name" (lisp-eval-tool-name)
             "description" (lisp-eval-tool-description)
             "parameters" (lisp-eval-tool-parameters)))))

;;; --- talking to the model ----------------------------------------------

(defun call-model (messages)
  (http-post-json
   *endpoint*
   `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
     ("content-type" . "application/json"))
   (obj "model" *model*
        "messages" (coerce (cons (obj "role" "system" "content" system-prompt)
                                 messages)
                           'vector)
        "tools" *tools*)))

;;; --- the loop itself ----------------------------------------------------
;;; An agent is a recursive function over a growing list of messages.
;;; Base case: the model answers in words. Recursive case: it asks
;;; for tools, we run them, and recur with the enriched history.

(defun agent-loop (messages)
  "Returns the complete message history, final answer included."
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
		*system-message* '())
  (remember-agent "AGENT-CHATGPT")
  (set-status STATUS-OK)
  *model*)

(defun forget ()
  (forget-mem MEMORY-FILE))

(defun list-models ()
  (unless *models*
    (setf *models*
            (gethash "data"
                     (http-get-json "https://api.openai.com/v1/models"
                                    :headers `(("content-type" . "application/json")
                                               ("Authorization" . ,(format nil "Bearer ~a" *api-key*))))))))

(defun lm ()
  (list-models)
  (print-model-ids *models*))

(defun llm ()
  (list-models)
  (loop for p across *models*
		for index from 1
		do
		   (maphash (lambda (k v)
					  (format t "~&~a~a: ~a~%"
							  (if (string-equal k "id") (format nil "[~a] " index) "")
							  k v))
					p)
		   (format t "~&__________~%")))

(defun set-model (num)
  (list-models)
  (setf *model* (model-id *models* num))
  (use))

(defun usage (&optional date)
  "Report this account's usage. The last model call's token counts come
first, when there was one -- what that turn cost -- then GET-USAGE's
account report: the provider's own balance, daily tokens or identity.
DATE, a \"YYYY-MM-DD\" string, picks the day and defaults to today."
  (openai-utils:print-usage-tokens *last-usage*)
  (openai-utils:get-usage :openai *api-key* :date date))
