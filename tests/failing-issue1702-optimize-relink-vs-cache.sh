#!/usr/bin/env bash
## Test for issue1702 - an optimize --relink does not relink the files
## in ~/.cache/darcs.
##
## Copyright (C) 2009  Trent W. Buck
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib

rm -rf R S
darcs init R

## Create a patch.
echo 'Example content.' > R/f
darcs record -lam 'Add f.' --repodir R

## Get a hard link into the cache.
darcs get R S

inode() {
  stat -c %i $1
}

# number of hard links
links() {
  stat -c %h $1
}

same_inode () {
  test $(inode $1) = $(inode $2)
}

## Are hard links available?
rm -f x y
touch x
if ! (ln x y && same_inode x y && test $(links x) = 2 && test $(links y) = 2); then
    echo This test requires filesystem support for hard links.
    exit 200
fi

inR=(R/_darcs/patches/*-*)
inS=(S/_darcs/patches/*-*)
patch=$(basename $inR)
inC=$(find $HOME/.cache/darcs/patches -name $patch)

## Confirm that all three are hard linked.
same_inode $inR $inS
same_inode $inS $inC
same_inode $inC $inR
# double check
test $(links $inR) = 3
test $(links $inS) = 3
test $(links $inC) = 3

## Break all hard links.
rm -rf S
cp -r R S
rm -rf R
cp -r S R

## Confirm that there are no hard links.
not same_inode $inR $inS
not same_inode $inS $inC
not same_inode $inC $inR
# double check
test $(links $inR) = 1
test $(links $inS) = 1
test $(links $inC) = 1

## Optimize *should* hard-link all three together.
darcs optimize relink --repodir R --sibling S

## Confirm that all three are hard linked.
same_inode $inR $inS
same_inode $inS $inC
same_inode $inC $inR
# double check
test $(links $inR) = 3
test $(links $inS) = 3
test $(links $inC) = 3
