(defpackage :common
  (:use :cl :utils :cl-ansi-text :uiop)
  (:export #:SYSTEM-PROMPT #:*CURRENT-RUN-FN* #:*current-model* #:*MEMORY-FILE* #:*SYSTEM-MESSAGE* #:SEP #:GREY #:RECALL
		   #:REMEMBER #:FORGET-MEM #:FORGET-ALL #:BASH #:CD #:SET-STATUS #:STATUS-THINKING #:STATUS-OK #:print-model-ids #:model-id
		   #:GET-PROMPT #:EP #:RP #:P #:ENP #:NP #:R #:HELP)
  (:nicknames :c :co))

(in-package :common)

(defconstant SYSTEM-PROMPT "You are a helpful agent with a live Common Lisp REPL. Prefer computing answers with lisp-eval over guessing. Your conversation history persists across sessions. You live in a Docker container without sudo or root access. Ask if you need a software to perform a task.")

(defparameter *current-run-fn* nil)
(defparameter *current-model* "")

(defparameter *editor* "/usr/bin/vi")
(defconstant PROMP_PATH "/agent/data/prompt.md")

(defconstant SEP (green  "___________________________________________________________________________"))
(defconstant STATUS-THINKING " thinking...")
(defconstant STATUS-OK "")

(defmacro defalias (alias original)
  "Make ALIAS share ORIGINAL's function object, so calling ALIAS doesn't add an extra funcall."
  `(setf (fdefinition ',alias) (fdefinition ',original)))

(defun grey (string)
  "cl-ansi-text only ships the 8 basic ANSI colors, none of them grey, so this
needs a wider palette. :24bit (truecolor, 38;2;r;g;b) looks right in a
directly-attached terminal but many terminal emulators/multiplexers -- tmux
or screen without an explicit Tc/RGB override, older terminal apps, some
SSH/web terminals -- don't understand it and silently render the default
foreground color instead of grey. :8bit (the 256-color palette, 38;5;n) has
been near-universally supported since the late 90s, so it renders as grey
almost everywhere truecolor might silently fail."
  (let ((*color-mode* :8bit))
    (with-output-to-string (s)
      (with-color ("#808080" :stream s)
        (write-string string s)))))

;;; --- memory ---------------------------------------------------------------
;;; Messages are already a list of hash tables, i.e. already JSON.
;;; So memory is nothing more than writing that list down and reading it back.

(defparameter *memory-file*
  (pathname (or (uiop:getenv "AGENT_MEMORY") "/agent/data/memory.json")))

(defparameter *system-message* '())

(defun remember (messages)
  (with-open-file (out *memory-file* :direction :output :if-exists :supersede)
    (shasht:write-json (coerce messages 'vector) out))
  messages)

(defun recall ()
  (if (probe-file *memory-file*)
      (coerce (with-open-file (in *memory-file*) (shasht:read-json in)) 'list)
      *system-message*))

(defun forget-mem (&optional (memory-file *memory-file*))
  (let ((memory-file (pathname memory-file)))
    (when (probe-file memory-file) (delete-file memory-file))
    (format t "~&Memory wiped: ~a.~%~a~%" memory-file SEP)))

;; Package names, not literal SYMBOL-QUALIFIED::NAMES: common.lisp loads
;; before any agent package exists, so the reader would choke on a
;; package-qualified symbol at load time. FIND-PACKAGE/FIND-SYMBOL resolve
;; by name at call time instead, once everything is actually loaded.
(defparameter *agent-packages* '("AGENT-CLAUDE" "AGENT-GEMINI" "AGENT-OLLAMA" "AGENT-CLAUDECODE" "AGENT-DEEPSEEK" "AGENT-MISTRAL" "AGENT-CHATGPT"))


;;; --- help ---------------------------------------------------------------
;;; A short catalogue of the agents on the box. The aliases are read from
;;; each package's own nickname list at call time, so adding a nickname to
;;; an agent's DEFPACKAGE is enough to have it show up here.

(defparameter *agent-blurbs*
  '(("AGENT"            . "OpenRouter -- any model it front-ends")
    ("AGENT-CLAUDE"     . "Anthropic API, straight" )
    ("AGENT-GEMINI"     . "Google Gemini API")
    ("AGENT-OLLAMA"     . "Local models via the Ollama server")
    ("AGENT-CLAUDECODE" . "Drives the `claude` CLI (subscription auth)")
    ("AGENT-DEEPSEEK"   . "DeepSeek API")
    ("AGENT-MISTRAL"    . "Mistral API")
    ("AGENT-CHATGPT"    . "OpenAI API"))
  "One line about what each agent talks to, keyed by package name.")

(defparameter *command-groups*
  '(("Prompts"
     ("ep"   . "edit the pending prompt")
     ("enp"  . "edit a fresh prompt (discard any pending one)")
     ("rp"   . "run the pending prompt as it stands")
     ("p"    . "edit the prompt, then run it")
     ("np"   . "edit a fresh prompt, then run it")
     ("r"    . "alias of np"))
    ("Models"
     ("lm"       . "list this agent's models, numbered")
     ("llm"      . "list this agent's models with full details")
     ("set-model" . "switch to model number N: (set-model 3)"))
    ("Session"
     ("run"     . "send a prompt straight to the current agent: (run \"...\")")
     ("use"     . "make this agent the current one and report its model")
     ("forget"  . "wipe this agent's conversation memory"))
    ("Memory"
     ("recall"  . "the current conversation, as stored")
     ("remember" . "replace the stored conversation with the list given")
     ("forget-mem" . "delete one memory file (default: the current one)")
     ("forget-all" . "wipe every agent's memory"))
    ("Shell"
     ("bash"  . "drop into a bash shell")
     ("cd"    . "change directory: (cd \"/tmp\")")))
  "REPL commands shared by every agent, grouped by what they are for.
Each entry is (COMMAND . DESCRIPTION); COMMAND is the short name to type.")

;;; Some commands are aliases for the same function (R is NP, etc.), so the
;;; catalogue shows the canonical names and notes the aliases in their
;;; descriptions rather than listing a line for each spelling.
(defun agent-aliases (package-name)
  "The nicknames of PACKAGE-NAME that name an agent to type at the REPL,
sorted, or NIL when the package is not loaded. The package's full name is
excluded: it is shown separately, with the aliases listed under it."
  (let ((package (find-package package-name)))
    (when package
      (sort (remove (string-downcase package-name)
                    (mapcar #'string-downcase (package-nicknames package))
                    :test #'string=)
            #'string<))))

(defun print-agent-help (package-name)
  "Print one line for PACKAGE-NAME: its aliases, then the package's
blurb. A package that is not loaded at all is noted rather than
silently dropped."
  (let ((aliases (agent-aliases package-name))
        (blurb (cdr (assoc package-name *agent-blurbs* :test #'string=))))
    (cond
      ((null (find-package package-name))
       (format t "~&  ?  ~a (not loaded)~%" package-name))
      (t
       (format t "~&  ~a~@[  (~{~a~^, ~})~]~%"
               (string-downcase package-name) aliases)
       (when blurb (format t "       ~a~%" blurb))))))

(defun print-command-group (group)
  "Print one GROUP: its heading, then each command and what it does."
  (format t "~&~%  ~a:~%" (car group))
  (dolist (entry (cdr group))
    (format t "    ~a~16t~a~%" (car entry) (cdr entry))))

(defun print-commands ()
  "Print every shared command, grouped. This is the interface common.lisp
adds to each agent: the prompt helpers, and the run/model/memory verbs every
agent exports."
  (format t "~&~a~%Commands:~%" SEP)
  (dolist (group *command-groups*)
    (print-command-group group)))

(defun help ()
  "List the shared commands, then the agents available and the aliases
that select each one. Type a command at the REPL -- e.g. (r), (lm),
(set-model 3) -- or an agent alias -- e.g. (cc), (claude), (g)."
  (print-commands)
  (format t "~&~%~a~%Agents (call an alias to switch):~%" SEP)
  (dolist (package-name (cons "AGENT" *agent-packages*))
    (print-agent-help package-name))
  (format t "~a~%" SEP)
  (values))

(defun forget-all ()
  "Calls FORGET in every agent package instead of duplicating each one's own
cleanup by deleting /agent/data/memory-*.json files directly: that missed
whatever extra state a given agent also needs to clear, e.g.
agent-claudecode's own session file alongside its memory."
  (dolist (package-name *agent-packages*)
    (let* ((package (find-package package-name))
           (fn (and package (find-symbol "FORGET" package))))
      (when (and fn (fboundp fn))
        (funcall fn)))))

;;; Shell helper
(defun bash ()
  (sb-ext:run-program "/bin/bash" nil :output t :input t :search t))

(defun cd (&optional path)
  (when path
	(chdir path))
  (getcwd))

;;; Generic run
(defun run (prompt)
  (funcall *current-run-fn* prompt))

(defun memo ()
  (run "Write down in the ./data/knowledge.md file what you have learned so far to share it with other IA. Acknowledge and output nothing else."))

(defun learn-from-knowledge ()
  (run "Learn what you should know so far from the file ./data/knowledge.md. Acknowledge and output nothing else."))

(defun learn-from-skill (skill)
  (let ((skill-str (string-downcase skill)))
	(run (format nil "Learn what you should know on skill `~a` from the file ./skills/~a/SKILL.md. Acknowledge and output nothing else." skill-str skill-str))))

(defun learn (&optional skill)
  (if (not skill)
	  (learn-from-knowledge)
	  (learn-from-skill skill)))

;; Status functions
(defun set-status (status)
  (with-open-file (out "./data/status" :direction :output :if-exists :supersede)
	(format out "[AI:~a~a]" *current-model* status)))

;; Prompt helpers
(defun get-prompt ()
  (let ((prompt ""))
	(when (probe-file PROMP_PATH)
	  (with-open-file (in PROMP_PATH :direction :input)
		(loop for line = (read-line in nil nil)
			  while line
			  do (setf prompt (format nil "~a~&~a" prompt line)))))
	prompt))

(defun print-prompt (prompt)
  (format t "~a~%" (yellow prompt)))

(defconstant CANCEL-MARKER "!no!")

;; Lets a prompt written to the prompt file veto its own run: typing !no!
;; anywhere in it (e.g. after editing and changing your mind) skips RUN
;; instead of deleting the whole prompt.
(defun run-unless-cancelled (prompt)
  (if (search CANCEL-MARKER prompt)
	  (format t "~&Cancelled: prompt contains `~a`.~%~a~%" CANCEL-MARKER SEP)
	  (run prompt)))

;; Edit prompt
(defun edit-prompt ()
  (sb-ext:run-program *editor* `(,PROMP_PATH) :output t :input t :search t)
  (print-prompt (get-prompt)))

(defalias ep edit-prompt)

;; Edit new prompt
(defun edit-new-prompt ()
  (when (probe-file PROMP_PATH)
	(delete-file PROMP_PATH))
  (edit-prompt))

(defalias enp edit-new-prompt)

;; Run prompt
(defun run-prompt ()
  (let ((prompt (get-prompt)))
	(print-prompt prompt)
	(run-unless-cancelled prompt)))

(defalias rp run-prompt)

;; Edit and run prompt
(defun edit-run-prompt ()
  (edit-prompt)
  (run-unless-cancelled (get-prompt)))

(defalias p edit-run-prompt)

;; Edit and run new prompt
(defun edit-run-new-prompt ()
  (edit-new-prompt)
  (run-unless-cancelled (get-prompt)))

(defalias np edit-run-new-prompt)
(defalias r edit-run-new-prompt)


;;; --- model pickers -----------------------------------------------------
;;; The list/lm/set-model trio is per-provider only in how a model entry
;;; exposes its id: OpenAI-style agents use the "id" field, Gemini uses
;;; "name" (with a "models/" prefix to strip).  These two helpers take
;;; that accessor as arguments; each agent keeps its own LIST-MODELS.

(defun print-model-ids (models &key (id-key "id") (id-fn #'identity))
  "Print \"[n] id\" for each model in MODELS (a vector of JSON objects)."
  (loop for m across models
        for index from 1
        do (format t "~&[~a] ~a~%" index (funcall id-fn (gethash id-key m)))))

(defun model-id (models num &key (id-key "id") (id-fn #'identity))
  "Return the id of the NUM-th (1-based) model in MODELS."
  (funcall id-fn (gethash id-key (aref models (1- num)))))
