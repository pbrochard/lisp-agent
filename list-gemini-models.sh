#! /bin/sh

GEMINI_API_KEY=$(gpg -d ~/.authinfo.gpg 2> /dev/null | awk '/^machine generativelanguage.googleapis.com/ {print $6}')

curl -s "https://generativelanguage.googleapis.com/v1beta/models?key=$GEMINI_API_KEY" \
     -H 'Content-Type: application/json' | jq '[.models[] | {id: .name, display_name: .displayName, description: .description}]'
