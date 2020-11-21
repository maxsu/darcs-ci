#!/usr/bin/env bash
## Test for issue1401 - when two repos share a HUNK patch, but that
## patch's ADDFILE dependency is met by different patches in each
## repo, it becomes impossible to pull between the two repos.
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

# this test fails for darcs-1 and darcs-2 repos and we cannot fix it
skip-formats darcs-1 darcs-2

darcs initialize --repodir d/
darcs initialize --repodir e/
touch d/f d/g e/f
darcs record     --repodir d/ -lam 'Add f and g'
darcs record     --repodir e/ -lam 'Add f'
echo >d/f
darcs record     --repodir d/ -am 'Change f'
darcs pull       --repodir e/ -a d/ --allow-conflicts #no conflict mark-up
echo y | darcs obliterate --repodir e/ -ap 'Add f and g'
darcs pull       --repodir e/ -a d/
