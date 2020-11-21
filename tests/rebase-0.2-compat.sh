#!/usr/bin/env bash

## Test that we can upgrade rebase format 0.2 repos

. lib

rm -rf R log
unpack_testdata rebase-0.2

cd R
darcs rebase log 2>../log
grep 'Rebase in progress: 1 suspended patch' ../log

darcs rebase unsuspend -a

cat > file.expected << END
v v v v v v v
change 1
=============
change 2
*************
initial content
^ ^ ^ ^ ^ ^ ^
END

diff -u file.expected file

cd ..
