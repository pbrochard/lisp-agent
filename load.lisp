(ql:quickload '(:dexador :shasht) :silent t)

(load "common.lisp")
(load "agent.lisp")

(setf common:*current-run-fn* #'agent:run)
