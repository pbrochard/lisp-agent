#! /bin/bash

API_KEY_CLAUDE=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/api.anthropic.com/ {print $6}')
API_KEY_GEMINI=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine generativelanguage.googleapis.com/ {print $6}')
API_KEY_OPENROUTER=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine openrouter.ai/ {print $6}')

docker run -it --rm \
	   --network=host \
	   -e API_KEY_OPENROUTER=$API_KEY_OPENROUTER \
	   -e API_KEY_GEMINI=$API_KEY_GEMINI \
	   -e API_KEY_CLAUDE=$API_KEY_CLAUDE \
	   -v "$(pwd)/data:/agent/data" \
	   -v "$(pwd)/skills:/agent/skills" \
	   -v "$HOME/src:/agent/data/src/" \
	   lisp-agent
