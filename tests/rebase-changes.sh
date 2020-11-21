#!/bin/sh -e

# Basic test for rebase changes

. lib

rm -rf R
mkdir R
cd R
darcs init

echo 'first' > file
darcs rec -lam 'change 1'

echo 'second' > file
darcs rec -am 'change 2'

echo 'third' > file
darcs rec -am 'change 3'

# suspend change 3
echo 'yd' | darcs rebase suspend

darcs rebase changes | grep "change 3"
darcs rebase changes -s | grep "M ./file"
darcs rebase changes --verbose | grep "third"
# we shouldn't see any sign of change 1 or change 2
darcs rebase changes --verbose | not grep "first"

# this turns change 2 into a fixup, so
# subsequence rebase changes should report conflicts
echo 'yd' | darcs obliterate

darcs rebase changes | grep "change 3"
darcs rebase changes -s | grep "M\! ./file"
darcs rebase changes --verbose | grep "third"
# now we should see a conflict with change 2
# which removes the line "first"
darcs rebase changes --verbose | grep "first"
darcs rebase changes --verbose | grep "conflicts:"
