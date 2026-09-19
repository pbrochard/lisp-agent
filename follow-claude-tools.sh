#! /bin/sh

clear
echo "Following logs in $(pwd)/claudecode-tools.md"
echo "____"

tail -F -n 0 claudecode-tools.md | batcat --paging=never --language=md --unbuffered --plain
