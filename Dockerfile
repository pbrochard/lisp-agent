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
 && apt-get install -y --no-install-recommends sbcl ca-certificates curl rlwrap build-essential lynx nodejs poppler-utils \
 && rm -rf /var/lib/apt/lists/*

RUN corepack enable

COPY data/debs/* /debs/
RUN dpkg -i /debs/*.deb

# Accept build arguments
ARG UID=1000
ARG GID=1000

# Create group and user
RUN groupadd -g $GID agentuser && \
    useradd -m -u $UID -g $GID agentuser

WORKDIR /agent
RUN chown -R agentuser:agentuser /agent

# Switch to non-root user
USER agentuser

# Quicklisp, installed non-interactively and wired into the SBCL init file.
RUN curl -sO https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
 && rm quicklisp.lisp

# Bake the dependencies into the image so startup is instant.
RUN sbcl --non-interactive --eval '(ql:quickload (list :dexador :shasht) :silent t)'

COPY load.lisp common.lisp agent.lisp agent-gemini.lisp .
COPY agent-run.sh .

COPY skills/ ./skills/

# Keep memory.json inside a mountable directory so it survives the container.
ENV AGENT_MEMORY=/agent/data/memory.json
RUN mkdir -p /agent/data

# Load the agent and drop you at a live REPL. This is the "login".
#ENTRYPOINT ["sbcl", "--load", "agent.lisp"]
ENTRYPOINT ["/agent/agent-run.sh"]
