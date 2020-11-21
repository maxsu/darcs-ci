#!/usr/bin/env bash

## Test that darcs refuses to work on old-fashioned repositories
## for certain commands.
##
## Copyright (C) 2011 Guillaume Hoffmann
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

. lib                  # Load some portability helpers.

rm -rf old hashed
unpack_testdata many-files--old-fashioned-inventory
mv many-files--old-fashioned-inventory old

cd old
not darcs add
not darcs amend
not darcs annotate
not darcs apply
not darcs diff
not darcs dist
not darcs mark
not darcs move
not darcs pull
not darcs push
not darcs record
not darcs remove
not darcs repair
not darcs replace
not darcs revert
not darcs rollback
not darcs send
not darcs setpref
not darcs tag
not darcs test
not darcs unrecord
not darcs unrevert
cd ..

## test that darcs can clone, pull and send (from, to) a remote
## old-fashioned repository.

darcs clone old hashed
cd hashed

test -e _darcs/hashed_inventory # issue1110: clone hashed
rm -rf temp1 temp2

echo "yyyd" | darcs unpull

darcs pull -a

touch tralala
darcs add tralala
darcs rec -am "patch to be sent"
darcs send -aO

cd ..

# issue2253 - darcs log FILE should not build patch index in an OF repo
cd old
darcs log x

# issue1584 - darcs optimize --upgrade

echo x > foo # Unrecorded change
darcs optimize upgrade
darcs check
grep hashed _darcs/format
not grep darcs-2 _darcs/format
darcs whatsnew | grep 'hunk ./foo 1'
cd ..

rm -rf old hashed

## issue1248 - darcs doesn't handle darcs 1 repos with compressed
## inventories

unpack_testdata oldfashioned-compressed
cd oldfashioned-compressed
darcs optimize upgrade
darcs check
cd ..
rm -rf oldfashioned-compressed
