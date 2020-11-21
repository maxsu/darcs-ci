#!/bin/sh -e
##
## Test that remote operations on rebase-in-progress repos do /not/ fail
## but ignore the rebase patch
##
## Copyright (C) 2012-3 Ganesh Sittampalam
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

rm -rf R1 R2 R3 R4
mkdir R1
cd R1
darcs init
echo '1' > foo
darcs rec -lam "normal patch"
echo '2' > foo
darcs rec -am "suspended patch"
echo yny | darcs rebase suspend
cd ..

darcs get R1 R2
cd R2
# this fails because there is no rebase in progress
not darcs rebase unsuspend
cd ..

mkdir R3
cd R3
darcs init
darcs pull -a ../R1 2>&1
cd ..

mkdir R4
cd R4
darcs init
cd ../R1
darcs push -a ../R4
cd ../R4
# this fails because there is no rebase in progress
not darcs rebase unsuspend
cd ..

mkdir R5
cd R5
darcs init
cd ../R1
darcs send -a ../R5 -o bundle.dpatch
not grep "DO NOT TOUCH" bundle.dpatch
grep "normal patch" bundle.dpatch
not grep "suspended patch" bundle.dpatch
cd ..
