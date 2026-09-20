;;;; agent.lisp — a recursive agent loop in Common Lisp
;;;;
;;;; The agent's only tool is EVAL. Homoiconicity does the rest:
;;;; the model writes Lisp, the loop runs it, the result flows back.
;;;;
;;;; Usage:
;;;;   export API_KEY_DEEPSEEK=sk-...
;;;;   sbcl --load agent.lisp --eval '(agent:run "What is the 30th Fibonacci number? Compute it.")'
;;;;
;;;; Memory: the full conversation persists to memory.json between runs.
;;;;   (agent:run "My name is Jamie.")
;;;;   ...later, in a fresh process...
;;;;   (agent:run "What is my name?")   ; => it remembers
;;;;   (agent:forget)                   ; wipe the slate

(defpackage :agent-deepseek
  (:use :cl :utils :openai-utils :common)
  (:export #:run #:use #:forget #:list-models #:*models* #:lm #:llm #:set-model)
  (:nicknames :ds :deepseek))

(in-package :agent-deepseek)

(defparameter *endpoint* "https://api.deepseek.com/chat/completions")
(defparameter *model* "deepseek-chat")
(defparameter *api-key* (uiop:getenv "API_KEY_DEEPSEEK"))

(defparameter *models* nil)

(defconstant MEMORY-FILE "/agent/data/memory-deepseek.json")

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
  (shasht:read-json
   (dex:post *endpoint*
             :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                        ("content-type" . "application/json"))
             :content (shasht:write-json
                       (obj "model" *model*
                            "messages" (coerce (cons (obj "role" "system" "content" system-prompt)
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
    (format t "~&______~&~%~a~%~a ~a~%" (gethash "content" (car (last history))) SEP (grey *model*))
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
	(forget-mem)))

(defun list-models ()
  (unless *models*
	(setf *models* (gethash "data" (shasht:read-json
									(dex:get "https://api.deepseek.com/models"
											 :headers `(("content-type" . "application/json")
														("Authorization" . ,(format nil "Bearer ~a" *api-key*)))))))))

(defun lm ()
  (list-models)
  (loop for p across *models*
		for index from 1
		do
		   (maphash (lambda (k v)
					  (when (string-equal k "id")
						(format t "~&[~a] ~a~%" index v)))
					p)))

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
  (setf *model* (gethash "id" (aref *models* (- num 1))))
  (use))
