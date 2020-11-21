#!/bin/sh -e
##
## Checking what happens when we need to remove an add from pending
## after doing a move.
##
## Copyright (C) 2018 Ganesh Sittampalam
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

rm -rf temp1
mkdir temp1
cd temp1
darcs init

echo 'foo' > a
echo 'bar' > b

darcs add a b

mv a c

darcs rec --look-for-moves c -a -m"added c via a"

darcs whatsnew > got
cat >want <<EOF
addfile ./b
hunk ./b 1
+bar
EOF

diff want got

cd ..


rm -rf temp2
mkdir temp2
cd temp2
darcs init

echo 'foo' > a
echo 'bar' > b

darcs add a b

mv b c

darcs rec --look-for-moves c -a -m"added c via b"

darcs whatsnew > got
cat >want <<EOF
addfile ./a
hunk ./a 1
+foo
EOF

diff want got

cd ..
