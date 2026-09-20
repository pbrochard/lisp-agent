(ql:quickload '(:dexador :shasht :cl-ansi-text :local-time) :silent t)

(load "utils.lisp")
(load "http-utils.lisp")
(load "openai-utils.lisp")
(load "common.lisp")
(load "agent.lisp")
(load "agent-gemini.lisp")
(load "agent-claude.lisp")
(load "agent-claudecode.lisp")
(load "agent-ollama.lisp")
(load "agent-deepseek.lisp")
(load "agent-mistral.lisp")
(load "agent-chatgpt.lisp")

(claudecode:use)

