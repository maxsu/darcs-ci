#!/bin/sh -e
##
## Test that commands don't read too much of the repository
##
## Copyright (C) 2013 Ganesh Sittampalam
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

rm -rf repo
unpack_testdata laziness-cut

cd repo

# log
darcs log --last=1
# amend
echo 'baz' > bar
echo yyyy | darcs amend
darcs log --last=1
# unrecord
echo yd | darcs unrecord -o
# log --context
darcs log --context > ctx
# record
darcs record -am xxx
darcs log --last=1
# send
echo ydy | darcs send -o xxx.dpatch --context ctx .
# obliterate
echo yd | darcs obliterate -o yyy.dpatch --no-minimize
tail -n +7 xxx.dpatch > zzz.dpatch
diff yyy.dpatch zzz.dpatch
# apply
darcs apply xxx.dpatch --debug
darcs log --last=1

cd ..
