#!/usr/bin/env bash

STATUS_FILE="./data/status"

function finish {
	rm -f $STATUS_FILE
    exit $1
}
trap 'finish $?' EXIT


echo "AI" > $STATUS_FILE

# Wrap SBCL in rlwrap for better command line editing and history support
# https://gist.github.com/vindarel/2309154f4e751be389fa99239764c363
rlwrap -r -i -b '()' --no-warnings sbcl --load load.lisp --eval '(in-package :common)'

