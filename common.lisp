(defpackage :common
  (:use :cl :utils :cl-ansi-text :uiop)
  (:export #:SYSTEM-PROMPT #:*CURRENT-RUN-FN* #:*current-model* #:*MEMORY-FILE* #:*SYSTEM-MESSAGE* #:SEP #:RECALL
		   #:REMEMBER #:FORGET-MEM #:FORGET-ALL #:BASH #:CD #:SET-STATUS #:STATUS-THINKING #:STATUS-OK #:print-model-ids #:model-id
		   #:GET-PROMPT #:EP #:RP #:P #:ENP #:NP #:R #:HELP #:REMEMBER-AGENT #:RECALL-AGENT #:USE-RECORDED-AGENT #:RECORDED-AGENT-NAME)
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

;;; --- current agent ---------------------------------------------------------
;;; Which agent USE last selected is remembered across processes, so a fresh
;;; LOAD.LISP comes back up on the agent you were just talking to instead of
;;; a hard-coded one. Only the package name is written: the agent's own USE
;;; rebuilds everything else (run function, model, memory file) on the way
;;; back in, so nothing but a name needs to survive.

(defparameter *agent-file*
  (pathname (or (uiop:getenv "AGENT_STATE") "/agent/data/agent"))
  "Where the name of the last agent selected with USE is kept.")

(defun remember-agent (package-name)
  "Record PACKAGE-NAME as the current agent. Called by each agent's USE."
  (with-open-file (out *agent-file* :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
    (format out "~a~%" package-name)))

(defun recall-agent ()
  "The package of the last agent selected, or NIL when none was ever
recorded (or the recorded name no longer names a package). FIND-PACKAGE
takes a nickname too, so a file hand-edited to \"cc\" still resolves."
  (when (probe-file *agent-file*)
    (with-open-file (in *agent-file*)
      (let ((name (string-trim '(#\Space #\Tab #\Newline)
                               (read-line in nil ""))))
        (and (plusp (length name)) (find-package name))))))

(defun use-recorded-agent (&optional (default "AGENT-CLAUDECODE"))
  "Select the agent last chosen with USE, falling back to DEFAULT (by name)
when nothing is recorded or the recorded one is gone. Print the agent
that ends up current, with the model USE returned, so a fresh load
says which agent it came up on. Returns the package now current."
  (let* ((package (or (recall-agent) (find-package default)))
         (use (and package (find-symbol "USE" package))))
    (if (and use (fboundp use))
        (progn (format t "~&~a~%~a ~a~%" SEP (grey (string-downcase (package-name package))) (funcall use))
               package)
        (let ((fallback (find-symbol "USE" (find-package default))))
          (warn "No usable agent recorded; falling back to ~a." default)
          (format t "~&~a~%~a ~a~%" SEP (grey (string-downcase default)) (funcall fallback))
          (find-package default)))))

;; Package names, not literal SYMBOL-QUALIFIED::NAMES: common.lisp loads
;; before any agent package exists, so the reader would choke on a
;; package-qualified symbol at load time. FIND-PACKAGE/FIND-SYMBOL resolve
;; by name at call time instead, once everything is actually loaded.
(defparameter *agent-packages* '("AGENT-CLAUDE" "AGENT-GEMINI" "AGENT-OLLAMA" "AGENT-CLAUDECODE" "AGENT-DEEPSEEK" "AGENT-MISTRAL" "AGENT-CHATGPT"))

;;; --- help ---------------------------------------------------------------
;;; A short catalogue of what you can type at the REPL: first the commands
;;; common.lisp adds to every agent, then the agents themselves with the
;;; aliases that select each one. The aliases are read from each package's
;;; own nickname list at call time, so adding a nickname to an agent's
;;; DEFPACKAGE is enough to have it show up here.

(defparameter *agent-blurbs*
  '(("AGENT"            . "OpenRouter -- any model it front-ends")
    ("AGENT-CLAUDE"     . "Anthropic API, straight")
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
     ("p"    . "edit the prompt, then run it (type `!no!` in the prompt to prevent running)")
     ("np"   . "edit a fresh prompt, then run it")
     ("r"    . "alias of np"))
    ("Models"
     ("lm"        . "list this agent's models, numbered")
     ("llm"       . "list this agent's models with full details")
     ("set-model" . "switch to model number N: (set-model 3)"))
    ("Session"
     ("run"    . "send a prompt straight to the current agent: (run \"...\")")
     ("use"    . "make this agent the current one and report its model")
     ("forget" . "wipe this agent's conversation memory")
     ("usage"   . "report this account's usage: cost, balance or tokens"))
    ("Shell"
     ("bash" . "drop into a bash shell")
     ("cd"   . "change directory: (cd \"/tmp\")")))
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
  (format t "~&  ~a:~%" (car group))
  (dolist (entry (cdr group))
    (format t "    ~a~16t~a~%" (car entry) (cdr entry))))

(defun print-commands ()
  "Print every shared command, grouped by what it is for."
  (format t "~&~a~%Commands:~%" SEP)
  (dolist (group *command-groups*)
    (print-command-group group)))

;;; Every agent exports the shared verbs above; a few export more of their
;;; own (only agent-claudecode so far). Those are read from the package's
;;; exports at call time -- minus the shared set -- so nothing here needs
;;; updating when an agent gains a command.

(defparameter *shared-exports*
  '("RUN" "USE" "FORGET" "LM" "LLM" "SET-MODEL" "LIST-MODELS"
     "USAGE")
  "The verbs every agent exports; a command in this list is common
interface, not an agent's own, and so is left out of its extras.")

(defparameter *agent-command-blurbs*
  '(("LE"            . "list this agent's effort levels, numbered")
    ("LIST-EFFORTS"  . "as LE, but in full")
    ("SET-EFFORT"    . "switch to effort number N: (set-effort 3)")
    ("SET-TIMEZONE"  . "zone usage reset times show in: (set-timezone \"Asia/Tokyo\")")
    ("VERBOSE"       . "toggle the live tool trace")
    ("SET-VERBOSE"   . "turn the live tool trace on or off")
    ("USAGE"       . "report this account's usage: cost, balance or tokens"))
  "One line about an agent's own command, keyed by exported symbol name.")

(defun variable-name-p (name)
  "True for a symbol name that names a variable -- *FOO* or +FOO+ -- rather
than a function to call. Those are exported alongside an agent's commands but
are not things to type on their own, so HELP leaves them out."
(let ((n (length name)))
  (and (> n 1)
       (let ((first (char name 0)) (last (char name (1- n))))
         (or (and (char= first #\*) (char= last #\*))
             (and (char= first #\+) (char= last #\+)))))))

(defun agent-extras (package-name)
  "The symbols PACKAGE-NAME exports beyond the shared set, sorted, as
lower-case strings -- the commands this agent has and the others do not."
  (let ((package (find-package package-name)))
    (when package
      (sort (set-difference
             (loop for s being the external-symbols of package
                   for name = (symbol-name s)
                   unless (or (member name *shared-exports* :test #'string=)
                              (variable-name-p name))
                     collect (string-downcase name))
             *shared-exports* :test #'string=)
            #'string<))))

(defun print-agent-extras (package-name)
  "Print PACKAGE-NAME's own commands, one per line. Silence when it has
none: most agents are exactly the shared interface."
  (let ((extras (agent-extras package-name)))
    (when extras
      ;;(format t "~&~%  ~a:~%" (string-downcase package-name))
      (dolist (name extras)
        (let ((blurb (cdr (assoc (string-upcase name)
                                 *agent-command-blurbs* :test #'string=))))
          (format t "       ~a~@[~20t~a~]~%" name blurb))))))

(defun help ()
  "List the shared commands, then the agents available with the aliases
that select each one and any commands an agent adds of its own. Type a
command at the REPL -- e.g. (r), (lm), (set-model 3) -- or an agent
alias -- e.g. (cc), (claude), (g)."
  (print-commands)
  (format t "~&~%~a~%Agents (call an alias to switch):~%" SEP)
  (dolist (package-name (cons "AGENT" *agent-packages*))
    (print-agent-help package-name)
    (print-agent-extras package-name))
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
  (run "Write down in the /agent/data/knowledge.md file what you have learned so far to share it with other IA. Acknowledge and output nothing else."))

(defun learn-from-knowledge ()
  (run "Learn what you should know so far from the file /agent/data/knowledge.md. Acknowledge and output nothing else."))

(defun learn-from-skill (skill)
  (let ((skill-str (string-downcase skill)))
	(run (format nil "Learn what you should know on skill `~a` from the file ./skills/~a/SKILL.md. Acknowledge and output nothing else." skill-str skill-str))))

(defun learn (&optional skill)
  (if (not skill)
	  (learn-from-knowledge)
	  (learn-from-skill skill)))

;; Status functions
(defun recorded-agent-name (&optional (default "AGENT-CLAUDECODE"))
  "The name of the agent USE-RECORDED-AGENT would select, as a short
lower-case label: the package name with its leading AGENT- dropped, so
AGENT-DEEPSEEK reads as \"deepseek\". Read non-destructively from the same
source USE-RECORDED-AGENT uses (the agent file), so SET-STATUS can label the
status with the recorded agent without re-selecting it -- calling
USE-RECORDED-AGENT here would recurse, since every agent's USE calls
SET-STATUS."
  (flet ((short (name)
           (let ((name (string-downcase name)))
             (if (and (> (length name) 6) (string= "agent-" name :end2 6))
                 (subseq name 6)
                 name))))
    (short (if (recall-agent) (package-name (recall-agent)) default))))

(defun set-status (status)
  (with-open-file (out "/agent/data/status" :direction :output :if-exists :supersede)
	(format out "[AI:~a:~a~a]" (recorded-agent-name) *current-model* status)))

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
