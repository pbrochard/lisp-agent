#!/usr/bin/env bash

STATUS_FILE="/agent/data/status"

export PATH="/home/user/.local/bin:$PATH"

function finish {
    rm -f $STATUS_FILE
    exit $1
}
trap 'finish $?' EXIT


echo "[AI]" > $STATUS_FILE

run_sbcl () {
    DATE=$(date +"%FT%T" | sed 's/[-:]//g')

    mkdir -p /agent/data/sessions

    /agent/prepare-sbcl.sh

    # Bridge 127.0.0.1:27017 -> host mongo and 127.0.0.1:11434 -> host ollama;
    # lisp-agent's own network has no direct route to either (see run.sh).
    node /agent/skills/mongo/scripts/mongo-bridge.js >> /agent/data/mongo-bridge.log 2>&1 &
    node /agent/ollama-bridge.js >> /agent/data/ollama-bridge.log 2>&1 &

    # Wrap SBCL in rlwrap for better command line editing and history support
    # https://gist.github.com/vindarel/2309154f4e751be389fa99239764c363
    # rlwrap's own -l/--logfile writes the session log directly, one pty layer
    # thinner than wrapping the whole thing in `script` (which added a second
    # WINCH hop and doubled CRs in the log -- see session-out.log history).
    # To filter out colors:
    #   `tail -F session-out.log | ansifilter`
    #   `ansifilter session-out.log > session-out-mono.log`
    rlwrap -r -i -b '()' --no-warnings -l /agent/data/sessions/session-$DATE.log sbcl --load load.lisp --eval '(in-package :common)'
}

if [ -n "$ENTRYPOINT" ]; then
    $ENTRYPOINT
else
    run_sbcl
fi
