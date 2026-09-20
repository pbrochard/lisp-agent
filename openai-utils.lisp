(defpackage :openai-utils
  (:use :cl :utils)
  (:export #:execute))

(in-package :openai-utils)

;;; --- the tool call, OpenAI-compatible shape -----------------------------
;;; ChatGPT, DeepSeek and Mistral all speak the same tool-calling dialect:
;;; each tool_call carries its name and (JSON-encoded) arguments nested
;;; under "function", and the result is fed back as a "tool" message keyed
;;; by the call's id. Only the progress log line ever differed between the
;;; three agents, so the arrow is shared too.

(defun execute (tool-call)
  "Turn one tool_call from the model into a tool-result message."
  (let* ((name (ref tool-call "function" "name"))
         (args (shasht:read-json (ref tool-call "function" "arguments")))
         (result (run-lisp-eval-tool name args)))
    (format t "~&  ⤷ ~a => ~a~%" (gethash "form" args) result)
    (obj "role" "tool"
         "tool_call_id" (gethash "id" tool-call)
         "content" result)))
