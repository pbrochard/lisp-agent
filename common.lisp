(defpackage :common
  (:use :cl)
  (:export #:SYSTEM-PROMPT #:SEP))

(in-package :common)

(defconstant SYSTEM-PROMPT "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions. You live in a Docker container without sudo or root access. Ask if you need a software to perform a task.")

(defconstant SEP "___________________________________________________________________________")
