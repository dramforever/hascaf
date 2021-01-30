#!/usr/bin/env sh

cd "$(dirname "$0")"
cabal v2-install --overwrite-policy=always --installdir="$(pwd)/build"
