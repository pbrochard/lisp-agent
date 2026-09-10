#! /bin/sh

# Quicklisp, installed non-interactively and wired into the SBCL init file.
curl -sO https://beta.quicklisp.org/quicklisp.lisp \
	&& sbcl --non-interactive \
			--load quicklisp.lisp \
			--eval '(quicklisp-quickstart:install)' \
			--eval '(ql-util:without-prompting (ql:add-to-init-file))' \
	&& rm quicklisp.lisp

# Bake the dependencies into the image so startup is instant.
sbcl --non-interactive --eval '(ql:quickload (list :dexador :shasht) :silent t)'
