#! /bin/sh

clear
tail -F claudecode-tools.md | batcat --paging=never --language=md --unbuffered --plain
