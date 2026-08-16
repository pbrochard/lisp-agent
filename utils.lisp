(defpackage :utils
  (:use :cl)
  (:export #:hash-table-keys #:hash-table-values #:obj-to-string #:replace-all #:remove-prefix))

(in-package :utils)

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
