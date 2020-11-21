#!/usr/bin/env bash

. lib

rm -rf temp1 temp2

mkdir temp1
cd temp1
darcs init
test -d _darcs
not darcs init
cd ..

# Some tests for the repodir flag
mkdir temp2
darcs init --repodir temp2
test -d temp2/_darcs

# Checking that `darcs init x` works

not darcs init x y            # refuse when 2 arguments are given
not darcs init x --repodir y  # refuse for the same reason
darcs init x

rm -rf temp1 temp2 x

## issue1266 - attempting to initialize a repository inside
## another repository should cause a warning, because while perfectly
## legitimate, it is likely to be accidental.

rm -rf out
darcs init --repodir temp1
darcs init --repodir temp1/temp2 2>&1 | tee out
grep -i WARNING out   # A warning should be printed.
rm -rf temp1 out
