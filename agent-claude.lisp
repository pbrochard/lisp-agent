;;;; agent.lisp — a recursive agent loop in Common Lisp
;;;;
;;;; The agent's only tool is EVAL. Homoiconicity does the rest:
;;;; the model writes Lisp, the loop runs it, the result flows back.
;;;;
;;;; Usage:
;;;;   export API_KEY=sk-ant-...
;;;;   sbcl --load agent.lisp --eval '(agent:run "What is the 30th Fibonacci number? Compute it.")'
;;;;
;;;; Memory: the full conversation persists to memory.json between runs.
;;;;   (agent:run "My name is Jamie.")
;;;;   ...later, in a fresh process...
;;;;   (agent:run "What is my name?")   ; => it remembers
;;;;   (agent:forget)                   ; wipe the slate

(defpackage :agent-claude
  (:use :cl :common)
  (:export #:run #:use)
  (:nicknames :cd :claude))

(in-package :agent-claude)

(defparameter *endpoint* "https://api.anthropic.com/v1/messages")
(defparameter *model* "claude-sonnet-4-6")
(defparameter *api-key* (uiop:getenv "API_KEY_CLAUDE"))
(defparameter *max-tokens* 4096)
(defparameter *api-version* "2023-06-01")

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
   (obj "name" "lisp-eval"
        "description" "Evaluate a Common Lisp form and return the printed result. Use this for computation, list manipulation, anything."
        "input_schema"
        (obj "type" "object"
             "properties" (obj "form" (obj "type" "string"
                                           "description" "A single Common Lisp form, e.g. (reduce #'+ (loop for i from 1 to 100 collect i))"))
             "required" (vector "form")))))

(defun execute (tool-use)
  "Turn one tool_use block from the model into a tool_result block."
  (let* ((name (gethash "name" tool-use))
         (args (gethash "input" tool-use))
         (result (if (string= name "lisp-eval")
                     (lisp-eval (gethash "form" args))
                     (format nil "ERROR: unknown tool ~a" name))))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "type" "tool_result"
         "tool_use_id" (gethash "id" tool-use)
         "content" result)))

;;; --- talking to the model ----------------------------------------------

(defun call-model (messages)
  (shasht:read-json
   (dex:post *endpoint*
             :headers `(("x-api-key" . ,*api-key*)
                        ("anthropic-version" . ,*api-version*)
                        ("content-type" . "application/json"))
             :content (shasht:write-json
                       (obj "model" *model*
                            "max_tokens" *max-tokens*
							"cache_control" (obj "type" "ephemeral")
                            "system" system-prompt
                            "messages" (coerce messages 'vector)
                            "tools" *tools*)
                       nil))))

;;; --- the loop itself ----------------------------------------------------
;;; An agent is a recursive function over a growing list of messages.
;;; Base case: the model answers in words. Recursive case: it asks
;;; for tools, we run them, and recur with the enriched history.

(defun agent-loop (messages)
  "Returns the complete message history, final answer included."
  (let* ((content (gethash "content" (call-model messages)))
         (assistant (obj "role" "assistant" "content" content))
         (tool-uses (remove-if-not
                     (lambda (b) (string= (gethash "type" b) "tool_use"))
                     (coerce content 'list))))
    (if tool-uses
        (agent-loop (append messages
                            (list assistant)
                            (list (obj "role" "user"
                                       "content" (map 'vector #'execute tool-uses)))))
        (append messages (list assistant)))))

;;; --- entry point ------------------------------------------------------------

(defun final-text (message)
  "Concatenate the text blocks of an assistant message."
  (with-output-to-string (s)
    (loop for b across (gethash "content" message)
          when (string= (gethash "type" b) "text")
            do (write-string (gethash "text" b) s))))

(defun use () nil)

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user" "content" prompt)))))))
    (format t "~&______~&~%~a~%~a ~a~%" (final-text (car (last history))) SEP *model*)
	(set-status STATUS-OK)))

(defun use ()
  (setf *current-run-fn* #'run
		*memory-file* (pathname "/agent/data/memory-claude.json")
		*system-message* '())
  *model*)
