#!/usr/bin/env bash

## Test that we can upgrade rebase format 0.0 repos

. lib

rm -rf R log
unpack_testdata rebase-0.0

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

# In format 0.0 it is legal to store a suspended patch containing a conflict,
# whereas the plan for future formats is to unwind the conflict.
# So make sure that old repos can be updated successfully.

rm -rf R log
unpack_testdata rebase-0.0-conflict

cd R
darcs rebase log 2>../log
grep 'Rebase in progress: 1 suspended patch' ../log

darcs rebase unsuspend -a
cat > file.expected << END
v v v v v v v
initial content
=============
content A
*************
content B
^ ^ ^ ^ ^ ^ ^
END

diff -u file.expected file

cd ..
