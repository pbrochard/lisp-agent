(defpackage :utils
  (:use :cl :cl-ansi-text)
  (:export #:grey #:obj #:hash-table-keys #:hash-table-values #:obj-to-string #:lisp-eval #:run-lisp-eval-tool #:lisp-eval-tool-name #:lisp-eval-tool-description #:lisp-eval-tool-parameters #:replace-all #:ref #:remove-prefix #:round-to-1-decimal #:format-count))

(in-package :utils)

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

;;; --- tiny JSON helpers -------------------------------------------------
;;; shasht reads JSON objects as hash tables; OBJ builds them going out.

(defun obj (&rest kvs)
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on kvs by #'cddr
        do (setf (gethash k h) v)
        finally (return h)))

(defun hash-table-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-table-values (hash-table)
  (loop for value being the hash-values of hash-table collect value))

(defun obj-to-string (obj)
  (with-output-to-string (str)
	(maphash (lambda (k v)
			   (format str "~&  ~a: ~a" k
					   (if (equal (type-of v) 'HASH-TABLE)
						   (obj-to-string v)
						   v)))
			 obj)))

;;; --- the tool: a Lisp REPL ---------------------------------------------
(defun lisp-eval (form-string)
  "The agent's hands. Read a form, eval it, print what came back."
  (handler-case
      (format nil "~s" (eval (read-from-string form-string)))
    (error (e) (format nil "ERROR: ~a" e))))


;;; --- the tool, shared across agents ------------------------------------
;;; Each provider's EXECUTE extracts the tool name and args differently,
;;; but they all dispatch to the same LISP-EVAL tool. This is that common
;;; core: NAME is the requested tool, ARGS a hash-table of its arguments.

(defun run-lisp-eval-tool (name args)
  "Dispatch one tool call to LISP-EVAL; return its printed result."
  (if (string= name "lisp-eval")
      (lisp-eval (gethash "form" args))
      (format nil "ERROR: unknown tool ~a" name)))


;;; --- the lisp-eval tool, as advertised to the model ---------------------
;;; Every agent exposes the same single tool. Its name, description and
;;; JSON parameter schema are shared here; each agent wraps them in its
;;; own provider-specific envelope ("function"/"parameters", Gemini's
;;; "function_declarations", Claude's "input_schema", ...).

(defun lisp-eval-tool-name ()
  "The name the model calls to run Lisp."
  "lisp-eval")

(defun lisp-eval-tool-description ()
  "What LISP-EVAL does, in words the model understands."
  "Evaluate a Common Lisp form and return the printed result. Use this for computation, list manipulation, anything.")

(defun lisp-eval-tool-parameters ()
  "The JSON-schema object describing LISP-EVAL's single FORM argument."
  (obj "type" "object"
       "properties"
       (obj "form"
            (obj "type" "string"
                 "description"
                 "A single Common Lisp form, e.g. (reduce #'+ (loop for i from 1 to 100 collect i))"))
       "required" (vector "form")))

;;; --- String helpers ----------------------------------------------------
(defun replace-all (string part replacement)
  "Replace all occurrences of PART in STRING with REPLACEMENT."
  (with-output-to-string (out)
    (loop with part-len = (length part)
          for old-pos = 0 then (+ pos part-len)
          for pos = (search part string :start2 old-pos)
          do (write-string string out :start old-pos :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun remove-prefix (string prefix)
  "Removes PREFIX from the start of STRING if it exists."
  (let ((len-p (length prefix))
        (len-s (length string)))
    (if (and (<= len-p len-s)
             (string= prefix string :end2 len-p))
        (subseq string len-p) ; Return string starting after the prefix
        string)))             ; Return the original string untouched

;;; --- numeric helpers -----------------------------------------------------

(defun round-to-1-decimal (x)
  (/ (round (* x 10)) 10.0))

(defun format-count (n)
  "N as a human-readable count, e.g. 1000000 => \"1M\", 985882 => \"985.9K\",
342 => \"342\". Mirrors how the Claude Code CLI abbreviates its own token
counts in its /context report."
  (flet ((trimmed (x)
           (let ((r (round-to-1-decimal x)))
             (if (= r (round r))
                 (format nil "~d" (round r))
                 (format nil "~,1f" r)))))
    (cond
      ((>= n 1000000) (format nil "~aM" (trimmed (/ n 1000000.0))))
      ((>= n 1000) (format nil "~aK" (trimmed (/ n 1000.0))))
      (t (format nil "~d" n)))))

;;; --- nested access helper ----------------------------------------------
;;; Walk nested hash tables / vectors. shasht reads JSON objects as hash
;;; tables and arrays as vectors; REF handles a mix of string keys
;;; (hash lookups) and integer indices (vector lookups).
;;;   (ref x "choices" 0 "message")   ; Gemini: (ref x "candidates" 0 "content")

(defun ref (table &rest keys)
  "Walk nested hash tables / vectors: (ref x \"choices\" 0 \"message\")"
  (reduce (lambda (acc key)
            (etypecase key
              (string (gethash key acc))
              (integer (aref acc key))))
          keys :initial-value table))
