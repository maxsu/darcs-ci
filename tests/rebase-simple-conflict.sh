#!/usr/bin/env bash

# Test for rebasing a conflict where we don't remove
# patches that are part of the conflict

. ./lib

rm -rf R S
darcs init R
cd R

echo 'initial' > file
darcs rec -lam 'initial'

cd ..
darcs clone R S
cd R

echo 'contentsA' > file
darcs rec -lam 'A'

cd ../S
echo 'contentsB' > file
darcs rec -lam 'B'

cd ../R
darcs pull ../S --mark-conflicts -a
# save the expected conflict markup
cp file file.expected
darcs rev -a # get rid of the markup
echo 'ydy' | darcs rebase suspend
echo 'yd' | darcs rebase unsuspend

diff -u file.expected file


