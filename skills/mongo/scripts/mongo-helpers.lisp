;; Here are the current definitions of the two helper functions as they exist in the Lisp REPL:
;;
;; ### `mongo-eval`
;; This is the base function that handles the shell execution. It uses a backquote to construct the command list, ensuring the expression is passed correctly as a string argument to `mongosh`.

(defun mongo-eval (expr)
  "Executes a MongoDB shell expression and returns the output string."
  (uiop:run-program
   `("mongosh" "--quiet" "--eval" ,expr)
   :output :string))

;; ### `mongo-query`
;; This is a higher-level wrapper designed for simple read operations. It automatically handles switching to the correct database, targeting a collection, and converting the cursor result into a JSON array via `.toArray()`.

(defun mongo-query (db collection query)
  "Runs a find query on a specific collection in a database."
  (let ((expr (format nil "db = db.getSiblingDB('~a'); db.~a.find(~a).toArray()"
					  db collection query)))
	(mongo-eval expr)))
