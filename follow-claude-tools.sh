#! /bin/sh

clear
tail -F -n 200 claudecode-tools.md | batcat --paging=never --language=md --unbuffered --plain
