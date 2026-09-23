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

# lisp-agent hands EVAL to an unsupervised model, so nothing in its own
# process environment should be a real API key -- it would just be a value
# for that model to read and try to exfiltrate. The real keys instead live
# only in keyproxy: lisp-agent's code only ever calls $KEYPROXY_URL, never a
# provider host directly, so keyproxy is the sole holder of real credentials
# regardless of lisp-agent's own network access. See
# keyproxy/nginx.conf.template for the six pinned upstream hosts.
#
# lisp-agent keeps normal internet access on this network (as it always
# has) -- eval shelling out to lynx/curl/git is an intended capability, not
# an accident, and cutting it off would protect nothing here: there is no
# key in this container to exfiltrate either way.
NETWORK=lisp-agent-net

docker rm -f keyproxy > /dev/null 2>&1
# Recreated every run rather than reused: a network left over from an
# earlier version of this script (e.g. one made --internal) would otherwise
# silently keep whatever flags it was created with.
docker network rm "$NETWORK" > /dev/null 2>&1
docker network create "$NETWORK" > /dev/null

# No --rm here: if keyproxy dies on startup we want `docker logs` to still
# have something to show below, instead of it vanishing and leaving
# lisp-agent to fail on an unresolvable hostname with no clue why.
docker run -d --name keyproxy \
	   --network "$NETWORK" \
	   -e API_KEY_OPENROUTER=$API_KEY_OPENROUTER \
	   -e API_KEY_GEMINI=$API_KEY_GEMINI \
	   -e API_KEY_CLAUDE=$API_KEY_CLAUDE \
	   -e API_KEY_DEEPSEEK=$API_KEY_DEEPSEEK \
	   -e API_KEY_MISTRAL=$API_KEY_MISTRAL \
	   -e API_KEY_OPENAI=$API_KEY_OPENAI \
	   keyproxy:latest > /dev/null

cleanup() { docker rm -f keyproxy > /dev/null 2>&1; }
trap cleanup EXIT

# nginx binds its listening port synchronously during startup, before the
# entrypoint even gets to "start worker processes" -- so a container still
# in state Running a moment later has the port open. No probe tool (nc,
# wget, ...) is assumed to exist inside the image, since nginx:alpine
# doesn't ship one.
sleep 1
if [ "$(docker inspect -f '{{.State.Running}}' keyproxy 2>/dev/null)" != "true" ]; then
	echo "keyproxy exited on startup:"
	docker logs keyproxy
	exit 1
fi
echo "keyproxy up."

docker run -it --rm \
	   --network "$NETWORK" \
	   -e KEYPROXY_URL=http://keyproxy:8080 \
	   -e CLAUDE_CODE_OAUTH_TOKEN=$CLAUDE_CODE_OAUTH_TOKEN \
	   -v "$(pwd)/data:/agent/data" \
	   -v "$(pwd)/skills:/agent/skills" \
	   -v "$HOME/src:/agent/data/src/" \
	   lisp-agent
