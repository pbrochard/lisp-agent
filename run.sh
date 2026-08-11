#! /bin/bash

#API_KEY=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/api.anthropic.com/ {print $6}')
#API_KEY=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine generativelanguage.googleapis.com/ {print $6}')
API_KEY=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine openrouter.ai/ {print $6}')

# -v /home/phil/src/adam:/agent/data/src/adam


docker run -it --rm \
	   -e API_KEY=$API_KEY \
	   -v "$(pwd)/data:/agent/data" \
	   -v "$(pwd)/skills:/agent/skills" \
	   lisp-agent
