#!/usr/bin/env bash

. ./lib

# rollback nothing

rm -rf temp1
darcs init temp1
cd temp1
date > file1
darcs record -lam "test"
rm file1
darcs record -am "rm"
echo yYd | tr [A-Z] [a-z] | darcs rollback --last=1 | grep 'No changes selected'
cd ..
rm -rf temp1

# issue1848 - interactive selection of primitive patches
# should still work with rollback -p

darcs init temp1
cd temp1
echo 'f' > f
echo 'g' > g
darcs record -lam 'Add f and g'
echo ynq | darcs rollback -p 'f and g'
cd ..
rm -rf temp1

# issue2242 - rollback of a mv patch generates bogus changes

darcs init temp1
cd temp1

# Setup dir with empty file in it
mkdir A
touch A/foo
darcs rec -alm 'Add A'

# Mv dir and add content to file
darcs mv A B
echo -e 'line1\nline2' > B/foo
darcs rec -alm 'Move A -> B and change foo'

# Rollback everything in the move/change patch
echo ynya | darcs roll

# We shouldn't see any rm'd dirs/files (just a move and line removal hunk)
darcs wh | not grep rm
cd ..
rm -rf temp1
