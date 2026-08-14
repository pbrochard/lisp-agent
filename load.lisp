(ql:quickload '(:dexador :shasht) :silent t)

(load "common.lisp")
(load "agent.lisp")
(load "agent-gemini.lisp")

(setf common:*current-run-fn* #'agent:run)
