#!/usr/bin/env bash

STATUS_FILE="/agent/data/status"

export PATH="/home/user/.local/bin:$PATH"

function finish {
	rm -f $STATUS_FILE
    exit $1
}
trap 'finish $?' EXIT


echo "[AI]" > $STATUS_FILE

/agent/prepare-sbcl.sh

# Bridge 127.0.0.1:27017 -> host mongo, unreachable directly since the keyproxy network change
node /agent/skills/mongo/scripts/mongo-bridge.js >> /agent/data/mongo-bridge.log 2>&1 &

# Wrap SBCL in rlwrap for better command line editing and history support
# https://gist.github.com/vindarel/2309154f4e751be389fa99239764c363
# To filter out colors:
#   `tail -F session-out.log | ansifilter`
#   `ansifilter session-out.log > session-out-mono.log`
script -q -f -c "rlwrap -r -i -b '()' --no-warnings sbcl --load load.lisp --eval '(in-package :common)'" /agent/data/session-out.log
