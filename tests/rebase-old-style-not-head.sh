#!/usr/bin/env bash

## Test that we can upgrade an old-style rebase repo where the
## special rebase container patch is not at the head of the repo

. lib

rm -rf R log
unpack_testdata old-style-rebase-not-head

cd R
darcs rebase upgrade
darcs rebase log 2>../log
grep 'Rebase in progress: 1 suspended patch' ../log
echo yd | darcs rebase unsuspend

# check that the suspended patch was properly commuted
# with the patch that was added after it was suspended
cat > file.expected << EOF
1
2
3
A
B
C2
D
EOF
diff -u file.expected file

cd ..
