(defpackage :utils
  (:use :cl :cl-ansi-text :sb-thread)
  (:export #:grey #:obj #:hash-table-keys #:hash-table-values #:obj-to-string #:lisp-eval #:run-lisp-eval-tool #:lisp-eval-tool-name #:lisp-eval-tool-description #:lisp-eval-tool-parameters #:replace-all #:ref #:remove-prefix #:round-to-1-decimal #:format-count
           #:with-timing #:format-timing #:reset-timings #:report-timings))

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
;;; --- timing ----------------------------------------------------------------
;;; Measure how long each agent's model work takes, per invocation, so the
;;; cost of a RUN can be compared across agent types. The agent type is passed
;;; in by the caller (each agent's RUN knows its own RECORDED-AGENT-NAME);
;;; utils loads before any agent, so it must not depend on common.
;;;
;;; Two clocks are kept, per invocation:
;;;   :wall  get-internal-real-time  -- everything, including waiting on the
;;;                                     network, which is most of a turn
;;;   :run   get-internal-run-time   -- CPU only, GC and blocking excluded
;;; Invocations are recorded rather than aggregated in place: a sum cannot be
;;; un-summed back into a distribution, and the raw samples are cheap.

(defparameter *timings* (make-hash-table :test #'equal)
  "Agent type (a string) -> a list of invocations, newest first, each
(:wall UNITS :run UNITS).")

(defparameter *timings-lock* (make-mutex :name "utils-timings")
  "Serializes pushes into *TIMINGS*: agent-claudecode's RUN can be reached
from more than one thread, so there is no single-threaded assumption here.")

(defun record-timing (agent-type wall run)
  "Add one invocation of AGENT-TYPE, both times in internal time units."
  (with-mutex (*timings-lock*)
    (push (list :wall wall :run run)
          (gethash agent-type *timings*))))

(defun reset-timings ()
  "Discard every recorded invocation."
  (with-mutex (*timings-lock*)
    (clrhash *timings*)))

(defun seconds (units)
  "Internal time UNITS as seconds, a float."
  (/ units (float internal-time-units-per-second 1.0)))

(defun format-duration (seconds)
  "SECONDS as a short readable string: a whole-second count above one
second, milliseconds below it, so a fast call does not read as 0.46s and a
slow one does not read as 8321ms."
  (if (>= seconds 1.0)
      (format nil "~,2fs" seconds)
      (format nil "~dms" (round (* seconds 1000)))))

(defmacro with-timing ((agent-type) &body body)
  "Run BODY as one invocation of AGENT-TYPE (a string), timing it with both
clocks, and return BODY's value unchanged."
  (let ((type (gensym "TYPE")) (t0 (gensym "T0")) (r0 (gensym "R0")))
    `(let ((,type ,agent-type)
           (,t0 (get-internal-real-time))
           (,r0 (get-internal-run-time)))
       (multiple-value-prog1
           (progn ,@body)
         (record-timing ,type
                        (- (get-internal-real-time) ,t0)
                        (- (get-internal-run-time) ,r0))))))

(defun format-timing (agent-type)
  "The last invocation of AGENT-TYPE as one line -- wall then cpu time -- or
NIL when it has never been timed. What a RUN prints on its closing line."
  (let ((samples (gethash agent-type *timings*)))
    (when samples
      (let* ((latest (first samples))
             (wall (seconds (getf latest :wall)))
             (run (seconds (getf latest :run))))
        (format nil "~a wall, ~a cpu"
                (format-duration wall) (format-duration run))))))

(defun sample-line (agent-type)
  "A compact summary of AGENT-TYPE's invocations: count, wall total/mean/
min/max, and mean cpu."
  (let* ((samples (gethash agent-type *timings*))
         (n (length samples)))
    (when (plusp n)
      (let* ((walls (mapcar (lambda (s) (seconds (getf s :wall))) samples))
             (runs  (mapcar (lambda (s) (seconds (getf s :run))) samples))
             (sum (lambda (xs) (reduce #'+ xs)))
             (mean (lambda (xs) (/ (funcall sum xs) n))))
        (format nil "~a: ~d run~:p, wall ~a total, ~a mean, ~a min, ~a max | cpu ~a mean"
                agent-type n
                (format-duration (funcall sum walls))
                (format-duration (funcall mean walls))
                (format-duration (apply #'min walls))
                (format-duration (apply #'max walls))
                (format-duration (funcall mean runs)))))))

(defun report-timings ()
  "Print one summary line per agent type that has been timed; return the
count of agent types reported."
  (let ((types (sort (hash-table-keys *timings*) #'string<)))
    (if types
        (progn
          (dolist (type types) (format t "~&~a~%" (sample-line type)))
          (length types))
        (progn (format t "~&No timings recorded.~%") 0))))
