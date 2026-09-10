(ql:quickload '(:dexador :shasht :cl-ansi-text) :silent t)

(load "utils.lisp")
(load "common.lisp")
(load "agent.lisp")
(load "agent-gemini.lisp")
(load "agent-claude.lisp")
(load "agent-ollama.lisp")

(agent:use)
