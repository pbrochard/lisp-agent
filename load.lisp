(ql:quickload '(:dexador :shasht) :silent t)

(load "common.lisp")
(load "agent.lisp")
(load "agent-gemini.lisp")
(load "agent-claude.lisp")

(setf common:*current-run-fn* #'agent:run)
