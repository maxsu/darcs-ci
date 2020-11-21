#!/bin/sh -e

# Check that rebase changes correctly identifies which
# parts of a change are in conflict and which aren't

. lib

rm -rf R
mkdir R
cd R
darcs init

echo contents1A > file1
echo contents2A > file2
echo contents3A > file3
echo contents4A > file4
darcs rec -lam "init"

echo contents1B > file1
echo contents3B > file3
darcs rec -am "change 1"

echo contents2B > file2
echo contents4B > file4
darcs rec -am "change 2"

echo contents1C > file1
echo contents2C > file2
echo contents3C > file3
echo contents4C > file4
darcs rec -am "change 3"

# suspend change 3 and obliterate change 2
# this should leave change 3 with fixups
echo "yd" | darcs rebase suspend
echo "yd" | darcs obliterate

darcs rebase changes -s | grep "M ./file1"
darcs rebase changes -s | grep "M\! ./file2"
darcs rebase changes -s | grep "M ./file3"
darcs rebase changes -s | grep "M\! ./file4"

# this is what we expect to appear in the "conflicts" section
darcs rebase changes --verbose | not grep "contents1A"
darcs rebase changes --verbose | grep "contents2A"
darcs rebase changes --verbose | not grep "contents3A"
darcs rebase changes --verbose | grep "contents4A"
