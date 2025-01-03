#!/bin/bash

set -euo pipefail

kernel="$(uname -s)"
case "$kernel" in
  CYGWIN*) kernel=Windows ;;
esac

arch="$(uname -m)"
case "$arch" in
  arm64) arch=aarch64 ;;
esac

URL="https://github.com/FStarLang/FStar/releases/download/nightly/fstar-$kernel-$arch.tar.gz"
FILE="$(basename "$URL")"

# Get artifact
wget "$URL" -O "$FILE"
tar xzf "$FILE"

# Untar
rm -rf out
mkdir out
tar xzf "$FILE" -C out
rm "$FILE"

echo Done.
