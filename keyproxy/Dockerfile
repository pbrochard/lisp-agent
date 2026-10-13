# keyproxy holds the real provider API keys and is the only container with
# internet egress; lisp-agent talks only to it, never to a provider host
# directly. See nginx.conf.template and ../run.sh.

FROM nginx:1.27-alpine

COPY nginx.conf.template /etc/nginx/templates/default.conf.template

EXPOSE 8080
