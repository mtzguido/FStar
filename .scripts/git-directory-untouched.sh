#!/bin/bash

set -eu

# This is a helper to decide if a given directory is clean according to git.
# i.e., no changed files, no deletions, no additions.

DIR="$1"

# If there's any output, i.e. any file not in HEAD, fail
if git ls-files --others --exclude-standard -- $DIR | grep -q . &>/dev/null; then
    exit 1
fi

# If there's a diff in existing files, fail
if ! git diff --exit-code $DIR &>/dev/null; then
    exit 1
fi

exit 0
