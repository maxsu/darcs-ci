#!/bin/sh -e
##
## General tests for the conflict UI:
##  - conflict reporting on pull etc ("We have conflicts in the following files:")
##  - conflict reporting in summaries of changes ("M! foo.txt")
##  - conflict marking in files
##
## Copyright (C) 2016 Ganesh Sittampalam
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

mkdir R1
cd R1
darcs init
cat > file1 <<EOF
line1
EOF
darcs add file1
cat > file2 <<EOF
line1
line2
line3
EOF
darcs add file2
darcs rec -am "initial patch"

cd ..
darcs get R1 R2
cd R2

cat > file1 <<EOF
line1A
EOF
cat > file2 <<EOF
line1A
line2
line3
EOF

darcs rec -am "patch 1"

cd ../R1

cat > file1 <<EOF
line1B
EOF
cat > file2 <<EOF
line1
line2
line3B
EOF

darcs rec -am "patch 2"
darcs pull -a ../R2 2> pull-output
grep "conflicts in the following files" pull-output
grep "file1" pull-output
not grep "file2" pull-output

cat > file1-expected <<EOF
v v v v v v v
line1
=============
line1A
*************
line1B
^ ^ ^ ^ ^ ^ ^
EOF

diff -u file1-expected file1

darcs changes --summary -p "patch 1" > changes-output
grep "M! ./file1 -1 +1" changes-output
grep "M ./file2 -1 +1" changes-output

