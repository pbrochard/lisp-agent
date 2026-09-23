# Lisp agent in a box.
#
# Build:
#   docker build -t lisp-agent .
#
# Run (interactive REPL, memory persisted to host):
#   docker run -it --rm \
#     -e OPENROUTER_API_KEY=sk-or-... \
#     -v "$(pwd)/data:/agent/data" \
#     lisp-agent
#
# Then at the REPL:
#   (agent:run "What is the 30th Fibonacci number? Compute it, don't recall it.")
#   (agent:run "My name is Jamie.")
#   (agent:forget)

FROM debian:trixie-slim

RUN apt-get update \
	&& apt-get install -y --no-install-recommends sbcl ca-certificates curl rlwrap build-essential git jq vim \
	lynx nodejs node-corepack node-gyp poppler-utils faketime openjdk-21-jdk-headless \
	&& rm -rf /var/lib/apt/lists/*

RUN corepack enable

# Claude Code CLI, for agent-claudecode.lisp (subscription auth, no API key).
RUN corepack npm install -g @anthropic-ai/claude-code

COPY data/debs/* /debs/
RUN dpkg -i /debs/*.deb

# Accept build arguments
ARG UID=1000
ARG GID=1000

# Create group and user
RUN groupadd -g $GID user && \
    useradd -m -u $UID -g $GID user

WORKDIR /
RUN chown -R user:user /home/user

WORKDIR /agent

## Quicklisp, installed non-interactively and wired into the SBCL init file.
COPY prepare-sbcl.sh .

COPY load.lisp utils.lisp openai-utils.lisp http-utils.lisp common.lisp agent.lisp agent-gemini.lisp agent-claude.lisp agent-claudecode.lisp \
	agent-ollama.lisp agent-deepseek.lisp agent-mistral.lisp agent-chatgpt.lisp .
COPY agent-run.sh ollama-bridge.js .
RUN chmod a+x ./agent-run.sh ./prepare-sbcl.sh

COPY skills/ ./skills/

RUN chown -R user:user /agent

# Switch to non-root user
USER user

# Keep memory.json inside a mountable directory so it survives the container.
ENV AGENT_MEMORY=/agent/data/memory.json
RUN mkdir -p /agent/data

# Load the agent and drop you at a live REPL. This is the "login".
#ENTRYPOINT ["sbcl", "--load", "agent.lisp"]
ENTRYPOINT ["/agent/agent-run.sh"]
