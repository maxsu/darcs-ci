#!/usr/bin/env bash
## Test for issue1959 - if the index becomes unwritable,
## read-only commands such as 'darcs whatsnew' should not die.
## Commands that modify the repo should fail with an appropriate
## error message.
##
## Copyright (C) 2012 Owen Stephens
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
cd R

echo foo > foo
darcs rec -alm 'testing'
echo change > foo

trap "chmod -R +w ." EXIT

chmod -R -w .

# commands that don't take a lock should not
# access the index at all if passed --ignore-times

darcs check
darcs diff --last=1
#would work if we could tell it to create the file elsewhere:
#darcs dist
darcs log
darcs init ../S
darcs send ../S -a -o ../patch
darcs show authors
darcs show contents foo
darcs show files
darcs show tags
darcs whatsnew

# ...and output a warning message otherwise

#doesn't work yet, it accesses the index directly via readIndex:
#darcs check --no-ignore-times | grep "Warning, cannot access the index"
darcs diff --last=1 --no-ignore-times 2>../log
grep "Warning, cannot access the index" ../log
darcs whatsnew --no-ignore-times 2>../log
grep "Warning, cannot access the index" ../log

# made it writable again
chmod -R +w .

# check that we get a decent error message
# for commands that modify the repo
test_cannot_write_index () {
  not darcs record -l foo -am foo 2>record
  grep "Cannot write index" record
  not darcs obliterate 2>record
  grep "Cannot write index" record
  not darcs move foo bar 2>move
  grep "Cannot write index" move
  echo bla > foo
  not darcs add foo 2>add
  grep "Cannot write index" add
  not darcs remove foo 2>remove
  grep "Cannot write index" remove
  not darcs replace x y foo 2>remove
  grep "Cannot write index" remove
}

# ...if the index iself isn't writable (but _darcs is)
chmod -R -w _darcs/index

test_cannot_write_index

# made it writable again
chmod -R +w .

# ...and similarly with _darcs/index_invalid
# the touch+add+remove is so that _darcs/index_invalid exists
touch f
darcs add f
darcs remove f
chmod -R -w _darcs/index_invalid

test_cannot_write_index

cd ..
