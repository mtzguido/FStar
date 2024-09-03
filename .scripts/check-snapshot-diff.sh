#!/bin/bash

set -eu

# This is run by CI to check that the snapshot has not changed after
# bootstrapping. Must run from the root of the F* git repo.

SUCCESS=true

# If there's any output, i.e. any file not in HEAD, fail
if git ls-files --others --exclude-standard -- ocaml/*/generated | grep -q . &>/dev/null; then
    SUCCESS=false
fi

# If there's a diff in existing files, fail
if ! git diff --exit-code ocaml/*/generated &>/dev/null; then
    SUCCESS=false
fi

if ! $SUCCESS; then
	echo "*********************************************************" >&2
	echo " *** SNAPSHOT DIFF: the generated OCaml files changed after CI" >&2
	echo ""                                                          >&2
	echo "$ git status ocaml/*/generated"				 >&2
	git status ocaml/*/generated					 >&2
	echo ""                                                          >&2
	echo "$ git diff -a ocaml/*/generated"				 >&2
	git diff -a ocaml/*/generated					 >&2
	echo "*********************************************************" >&2
	exit 1
else
	exit 0
fi
