(ql:quickload '(:dexador :shasht :cl-ansi-text :local-time) :silent t)

(load "utils.lisp")
(load "common.lisp")
(load "agent.lisp")
(load "agent-gemini.lisp")
(load "agent-claude.lisp")
(load "agent-claudecode.lisp")
(load "agent-ollama.lisp")

(claudecode:use)
