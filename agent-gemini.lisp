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

(defpackage :agent-gemini
  (:use :cl :utils :common)
  (:export #:run #:use #:forget #:list-models :*model* :*models* #:lm #:llm #:set-model)
  (:nicknames :g :gm :gem :gemini))

(in-package :agent-gemini)

(defparameter *endpoint* "https://generativelanguage.googleapis.com/v1beta/models/")
(defparameter *model* "gemini-3.5-flash")
;;(defparameter *model* "gemini-3.1-pro-preview")
(defparameter *api-key* (uiop:getenv "API_KEY_GEMINI"))

(defparameter *models* nil)

(defconstant MEMORY-FILE "/agent/data/memory-gemini.json")

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
                            (obj "parts" (vector (obj "text" system-prompt)))
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

;;; --- entry point ------------------------------------------------------------

(defun final-text (message)
  "Concatenate the text parts of a model message."
  (with-output-to-string (s)
    (loop for p across (gethash "parts" message)
          for text = (gethash "text" p)
          when (and text (not (gethash "thought" p)))
            do (write-string text s))))

(defun use () nil)

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user"
                                      "parts" (vector (obj "text" prompt)))))))))
    (format t "~&______~&~%~a~%~a ~a~%" (final-text (car (last history))) SEP *model*)
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
	(setf *models* (gethash "models" (shasht:read-json
									  (dex:get (format nil "https://generativelanguage.googleapis.com/v1beta/models?key=~a" *api-key*)
											   :headers '(("content-type" . "application/json"))))))))

(defun lm ()
  (list-models)
  (loop for p across *models*
		for index from 1
		do
		   (maphash (lambda (k v)
					  (when (string-equal k "name")
						(format t "~&[~a] ~a~%" index (remove-prefix v "models/"))))
					p)))

(defun llm ()
  (list-models)
  (loop for p across *models*
		for index from 1
		do
		   (maphash (lambda (k v)
					  (format t "~&~a~a: ~a~%" (if (string-equal k "name")
												   (format nil "[~a] " index)
												   "")
							  k v))
					p)
		   (format t "~&__________~%")))

(defun set-model (num)
  (list-models)
  (setf *model* (remove-prefix (gethash "name" (aref *models* (- num 1))) "models/"))
  (use))
