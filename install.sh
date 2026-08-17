#!/usr/bin/env bash
# build henle and copy the binary somewhere on your PATH
set -euo pipefail

cd "$(dirname "$0")"

echo "Building..."
dune build

DEST="${1:-$HOME/.local/bin}"
mkdir -p "$DEST"

# remove existing binaries first rather than copying over it in place
# removing it first only needs folder-level permission, which is already there
if [ -e "$DEST/henle" ]; then
  rm -f "$DEST/henle" 2>/dev/null || {
    echo "Couldn't remove the existing $DEST/henle, it may be owned by another user."
    echo "Check with: ls -l $DEST/henle"
    echo "If it's owned by root (e.g. from an earlier 'sudo' install), remove it with:"
    echo "  sudo rm $DEST/henle"
    echo "then run this script again."
    exit 1
  }
fi

cp _build/default/bin/main.exe "$DEST/henle"
chmod +x "$DEST/henle"

echo "Installed to $DEST/henle"
if ! command -v henle >/dev/null 2>&1; then
  echo "Note: $DEST doesn't appear to be on your PATH."
  echo "Add this to your shell profile:"
  echo "  export PATH=\"$DEST:\$PATH\""
fi
