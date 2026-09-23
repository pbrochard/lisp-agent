#! /bin/sh

cls() {
	rows=$(tput lines 2>/dev/null) || rows=24
	i=0
	while [ "$i" -lt "$rows" ]; do
		printf '\n'
		i=$((i + 1))
	done
	tput cup 0 0 2>/dev/null
}

cls
echo "Following logs in $(pwd)/claudecode-tools.md"
echo "____"

tail -F -n 0 claudecode-tools.md | batcat --paging=never --language=md --unbuffered --plain
