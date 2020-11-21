#!/usr/bin/env bash
. ./lib

# tests for "darcs optimize"

## test that darcs optimize reorder works

rm -rf test1
mkdir test1
cd test1
darcs init
touch foo
darcs record -a -m add_foo -l foo
darcs tag foo_tag
# check tag is initially clean
grep 'Starting with inventory' _darcs/hashed_inventory
touch bar
darcs record -a -m add_bar -l bar
# make the tag unclean
echo y | darcs amend -p foo_tag -a --author me
not grep 'Starting with inventory' _darcs/hashed_inventory
darcs optimize reorder | grep -i "done"
# check it is again clean
grep 'Starting with inventory' _darcs/hashed_inventory
cd ..

## issue2388 - optimize fails if no patches have been recorded

rm -rf test2
darcs init test2
cd test2
darcs optimize clean
cd ..
