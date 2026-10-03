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
  (:export #:run #:use #:forget #:usage)
  (:nicknames :ol :ollama))

(in-package :agent-ollama)

(defparameter *endpoint* "http://172.17.0.1:11434/api/chat")
;;(defparameter *model* "qwen3:8b")
;;(defparameter *model* "deepseek-r1:latest")
(defparameter *model* "qwen3")

(defconstant MEMORY-FILE "/agent/data/memory-ollama.json")

(defparameter *last-usage* nil
  "The token counts the last model call reported, kept so USAGE can show
what a turn cost. NIL until a call has been made.")

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
    (format t "~&  ⤷ ~a~&    => ~a~%" (grey (gethash "form" args)) (grey result))
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
  (let* ((response (call-model messages))
         (message (gethash "message" response))
         (tool-calls (gethash "tool_calls" message)))
    (setf *last-usage* response)
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

;;; --- usage & token reporting ------------------------------------------
;;; Ollama returns a call's token counts at the top level of its reply, not
;;; under a usage object: prompt_eval_count for the input, eval_count for
;;; what it generated. There is no account billing -- the server is local --
;;; so this reports what a call cost, not what is left.

(defun format-ollama-tokens (response)
  "RESPONSE's token counts as one readable line, or NIL when there is
nothing to show. The style matches openai-utils:FORMAT-USAGE-TOKENS so a
run reads the same whichever agent ran it."
  (when response
    (let ((input (gethash "prompt_eval_count" response))
          (output (gethash "eval_count" response)))
      (when (or input output)
        (with-output-to-string (s)
          (write-string "Tokens:" s)
          (when input  (format s " ~a in" input))
          (when output (format s "~:[, ~; ~]~a out" (not input) output))
          (when (and input output) (format s " (~a total)" (+ input output))))))))

(defun usage (&optional date)
  "Report usage. The last model call's token counts come first, when there was
one -- what that turn cost. Ollama runs locally, so there is no account
balance or billing to add. DATE is accepted so (usage) is uniform across
agents and ignored."
  (declare (ignore date))
  (let ((line (format-ollama-tokens *last-usage*)))
    (if line
        (format t "~&~a~%" (grey line))
        (format t "~&No model call yet in this session.~%"))))

(defun run (prompt)
  (use)
  (set-status STATUS-THINKING)
  (let ((history (remember
                  (agent-loop
                   (append (recall)
                           (list (obj "role" "user" "content" prompt)))))))
    (format t "~&______~&~%~a~%" (final-text (car (last history))))
    (let ((tokens (format-ollama-tokens *last-usage*)))
      (when tokens (format t "~&~a~%" (grey tokens))))
    (format t "~&~a ~a:~a~%" SEP (grey (recorded-agent-name)) (grey *model*))
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
