#! /bin/sh

ANTHROPIC_API_KEY=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/api.anthropic.com/ {print $6}')

curl -s https://api.anthropic.com/v1/models \
     -H 'anthropic-version: 2023-06-01' \
     -H "X-Api-Key: $ANTHROPIC_API_KEY" | jq '[.data[]  | {id, display_name, created_at}]'
