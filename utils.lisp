(defpackage :utils
  (:use :cl)
  (:export #:obj #:hash-table-keys #:hash-table-values #:obj-to-string #:lisp-eval #:run-lisp-eval-tool #:lisp-eval-tool-name #:lisp-eval-tool-description #:lisp-eval-tool-parameters #:replace-all #:ref #:remove-prefix))

(in-package :utils)

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
