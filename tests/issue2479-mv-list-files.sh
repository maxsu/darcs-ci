#!/usr/bin/env bash
## Test for issueNNNN - Bad error message in 'darcs move' if the repo root dir (".")
##                      is given among multiple sources for the move.

. lib                           # Load some portability helpers.

mkdir R
cd R
darcs init
echo "test" > b
darcs add b
darcs record -a -m 'added b'
mkdir a
not darcs move . ./b a 2>&1 | not grep -i bug
