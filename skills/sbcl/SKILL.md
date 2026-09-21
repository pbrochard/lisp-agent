# SBCL Skill

Practical notes for driving SBCL 2.5.2 from a REPL-style tool, where the only
thing you get back is the **value of the form you submit** — not anything the
form prints. Learned the hard way while editing the agents in this repo.

## Golden rule: capture output as a value
The tool shows the *value* of the form. A bare `(format t ...)` prints to
`*standard-output*`, which is **not** captured — you just see `NIL`. To see
output, either `format nil` or wrap in `with-output-to-string`.
```lisp
;; invisible (returns NIL):
(format t "hello ~a~%" 42)
;; visible (returns the string):
(format nil "hello ~a" 42)
(with-output-to-string (s) (format s "hi ~a" 42))
```
Same applies to `write-line`, `print`, `princ`: they return NIL. Build a
string and return it, or write to a file and read it back.

## Multi-form input
Only the **last** form's value is shown. To define several things then use
them, define in one call and use in the next:
```lisp
;; call 1  (result shown is :OK)
(progn (defun f (x) (* x x)) (defun g (x) (1+ x)) :ok)
;; call 2
(f (g 4))   ; => 25
```

## Case folding
`(readtable-case *readtable*)` is `:UPCASE`, so `foo`, `Foo`, `FOO` all read as
the same symbol `FOO`. This bites when a name collides with a loaded constant:
```lisp
;; ERROR: SEP names a defined constant, and cannot be used in LET.
(let ((sep "---")) (format nil "~a" sep))
```
Pick names that can't collide (`sepstr`, `mdl`, ...).

## Inspecting the filesystem
`uiop` is available in the running image.
```lisp
(uiop:getcwd)                       ; => #P"/home/user/lisp-agent/"
(directory "*.lisp")              ; list matching paths
(probe-file "/etc/hosts")          ; path or NIL
(directory "/path/*/")             ; subdirs (trailing slash)
```
Note: `sb-ext:run-program` with `:output :string` gave NIL here. Output must go
to a **file** to be captured reliably (see next).

## Running a fresh SBCL to validate a whole project
In-process `load`/`compile-file` of already-loaded files trips
`DEFCONSTANT` redefinition errors. The clean end-to-end check is a **separate
process**:
```lisp
(progn
  (ignore-errors (delete-file "/tmp/out"))
  (sb-ext:run-program "sbcl"
    '("--non-interactive" "--load" "/path/load.lisp"
      "--eval" "(progn (format t \"~&LOADED-OK~%\") (sb-ext:exit :code 0))")
    :output "/tmp/out" :error "/tmp/err" :search t
    :environment (cons "API_KEY=test" (sb-ext:posix-environ)))
  :spawned)
;; then read /tmp/out and /tmp/err back with a slurp helper
```
`sb-ext:run-program` returns `#<PROCESS :EXITED 0>` on success. Redirect BOTH
`:output` and `:error` to files; read them back.

## slurp / spit (line-oriented file I/O)
```lisp
(defun slurp (path)
  (with-open-file (s path)
    (let (ls) (loop for l = (read-line s nil nil) while l do (push l ls))
      (nreverse ls))))
(defun spit (path lines)
  (with-open-file (o path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (l lines) (write-line l o))
    :ok))
```
Always **back up** before rewriting: `(spit (concatenate 'string path ".bak") (slurp path))`.

## Line endings
`read-line`/`write-line` normalise, so if CRLF matters check the raw bytes:
```lisp
(with-open-file (s path :element-type '(unsigned-byte 8))
  (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
    (read-sequence b s)
    (list :cr (count 13 b) :lf (count 10 b))))
```
This repo's `.lisp` files are LF-only.

## Syntax-check without side effects
Parse every top-level form with `read` — catches reader/paren errors without
evaluating or signalling `defconstant` complaints:
```lisp
(with-open-file (s path)
  (loop for f = (read s nil :eof) until (eq f :eof) count 1))  ; => form count
```

## Compile and muffle
```lisp
(handler-bind ((style-warning #'muffle-warning)
               (warning       #'muffle-warning))
  (compile-file path :output-file "/tmp/x.fasl"))
```
A `format` directive/argument count mismatch is only a **STYLE-WARNING**
("Too many arguments to FORMAT"), not an error — it still loads, so grep the
stderr file for `STYLE-WARNING` after a subprocess load.

## format quick facts
- `~a` = aesthetic (no quotes), `~s` = readable (quotes/escapes).
- `~&` fresh-line, `~%` newline, `~{~a ~}` iterate a list.
- `~:@(...)` / `~@[ ... ~]` conditionals; arg count must match directives.
- `(format nil "~s" x)` gives a string you can store/compare.

## Getting out / signalling
Inside a subprocess use `(sb-ext:exit :code 0)` so the run terminates cleanly
instead of dropping into the debugger on EOF.
