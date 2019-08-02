#! /usr/bin/env bash

# List GitHub issues associated with a repository. It is recomended that you
# provide an auth token in your .netrc.
#
# usage: github_issues.sh owner repo

: ${BASE_URL:=https://api.github.com}

OWNER="$1"
REPO="$2"

printf "%s/%s - " "$1" "$2"
curl -nsS "${BASE_URL}/repos/$OWNER/$REPO/issues" \
    | jq -r 'length,(.[] | "\(.title) | href=\(.html_url)")'

echo "---"

curl -nsS "${BASE_URL}/repos/$OWNER/$REPO" \
    | jq -r '"Go to repository | href=\(.html_url)"'

