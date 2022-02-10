#!/bin/bash

# Run this script in the top-level of your package directory
# (where the .cabal file is) to compile documentation and
# upload it to hackage.

# Requirements:
# cabal-install-1.24 (for --for-hackage)
# haddock 2.17 (for the hyperlinked source)

set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload  --publish -d $dir/*-docs.tar.gz
