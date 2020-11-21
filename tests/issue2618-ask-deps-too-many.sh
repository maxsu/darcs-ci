#!/usr/bin/env bash
# Test that --ask-deps adds no more than the specified explicit dependencies
# and not indirect (implicit) dependencies.

. ./lib

rm -rf R
darcs init R
cd R
echo text > file
darcs record -lam 'one'
echo othertext > file
darcs record -am 'two'
echo text > other_file
echo yd | darcs record -lam 'three' --ask-deps
darcs log -s --last=1 | tee LOG
# should depend on patch named 'two'
grep two LOG
# but not on patch named 'one'
not grep one LOG
cd ..
