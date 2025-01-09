#!/bin/bash

# set -x
set -eu

if [ $# -ne 2 ]; then
	echo "usage: $0 <actual_output> <expected_output>" >&2
	exit 1
fi

ACTUAL="$1"
EXPECTED="$2"

DIFF="diff -u --strip-trailing-cr"

if $DIFF "$ACTUAL" "$EXPECTED" ; then
  # OK
  echo ok
  exit 0
else
  # We're gonna fail, maybe emit a github message
  if [ -v GITHUB_ENV ]; then
    DIFFTEXT=$($DIFF "$ACTUAL" "$EXPECTED" | tr '\n' '%0A')
    ACTUAL=$(realpath "$ACTUAL")
    ACTUAL="${ACTUAL#$FSTAR_ROOT}"
    EXPECTED=$(realpath "$EXPECTED")
    EXPECTED="${EXPECTED#$FSTAR_ROOT}"
    echo "::error::Diff failed for files $ACTUAL and $EXPECTED:$DIFFTEXT"
  else
    echo "error: Diff failed for files $ACTUAL and $EXPECTED" >&2
    # echo "$DIFFTEXT"
  fi
  exit 1
fi
