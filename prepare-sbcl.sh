#! /bin/sh

if [ ! -f "/home/user/quicklisp/setup.lisp" ]; then
	# Quicklisp, installed non-interactively and wired into the SBCL init file.
	curl -sO https://beta.quicklisp.org/quicklisp.lisp \
		&& sbcl --non-interactive \
				--load quicklisp.lisp \
				--eval '(quicklisp-quickstart:install)' \
				--eval '(ql-util:without-prompting (ql:add-to-init-file))' \
		&& rm quicklisp.lisp

	# Bake the dependencies into the image so startup needs no network: since
	# lisp-agent now runs on a Docker network with no route to the internet (see
	# run.sh, keyproxy/), anything load.lisp quickloads has to already be here.
	sbcl --non-interactive --eval '(ql:quickload (list :dexador :shasht :cl-ansi-text :local-time) :silent t)'
fi
