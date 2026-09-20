#! /bin/bash

API_KEY_CLAUDE=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/api.anthropic.com/ {print $6}')
API_KEY_GEMINI=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine generativelanguage.googleapis.com/ {print $6}')
API_KEY_OPENROUTER=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine openrouter.ai/ {print $6}')
API_KEY_DEEPSEEK=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine api.deepseek.com/ {print $6}')
API_KEY_MISTRAL=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine api.mistral.ai/ {print $6}')
# Long-lived token from `claude setup-token`, for agent-claudecode.lisp (subscription auth, no API key).
CLAUDE_CODE_OAUTH_TOKEN=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine claude-code/ {print $6}')
API_KEY_OPENAI=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine api.openai.com/ {print $6}')

# --network=host \

docker run -it --rm \
	   -e API_KEY_OPENROUTER=$API_KEY_OPENROUTER \
	   -e API_KEY_GEMINI=$API_KEY_GEMINI \
	   -e API_KEY_CLAUDE=$API_KEY_CLAUDE \
	   -e API_KEY_DEEPSEEK=$API_KEY_DEEPSEEK \
	   -e API_KEY_MISTRAL=$API_KEY_MISTRAL \
	   -e CLAUDE_CODE_OAUTH_TOKEN=$CLAUDE_CODE_OAUTH_TOKEN \
	   -e API_KEY_OPENAI=$API_KEY_OPENAI \
	   -v "$(pwd)/data:/agent/data" \
	   -v "$(pwd)/skills:/agent/skills" \
	   -v "$HOME/src:/agent/data/src/" \
	   lisp-agent
