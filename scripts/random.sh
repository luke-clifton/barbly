#! /usr/bin/env bash
date
echo  ---

cat /dev/urandom | base64 | head -n 30 | while read -r
do
    echo "$REPLY | href=https://www.google.com"
done
