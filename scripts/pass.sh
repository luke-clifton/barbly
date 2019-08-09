#! /usr/bin/env bash

: ${PASSWORD_STORE_DIR:="$HOME/.password-store"}

cd "$PASSWORD_STORE_DIR"
echo "Pass"
echo "---"

secrets() {
	local path="$1"
	local level="$2"
	for s in ${path}/*
	do
		if [ -f "$s" ]
		then
			echo "$level $(basename $s .gpg) | bash=pass param1=show param2=-c param3=$(dirname "$s")/$(basename "$s" .gpg)"
		elif [ -d "$s" ]
		then
			echo "$level $(basename $s)"
			secrets "$s/" "--$level"
		fi
	done
}

secrets "." ""
