#!/usr/bin/env bash
## ensure empty directories get deleted when importing from git
##
## Copyright (C) 2014 Owen Stephens
##               2016 Guillaume Hoffmann
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

! read -r -d '' DATA <<'EOF'
blob
mark :1
data 8
testing

reset refs/heads/master
commit refs/heads/master
mark :2
author CommiterName <me@example.org> 1307452813 +0100
committer CommiterName <me@example.org> 1307452813 +0100
data 10
add dir/a
M 100644 :1 dir/a

commit refs/heads/master
mark :3
author CommiterName <me@example.org> 1307452821 +0100
committer CommiterName <me@example.org> 1307452821 +0100
data 24
copy, rename, copy dirs
from :2
C "dir" "dir2"
R "dir" "dir3/dir4"
C "dir3/dir4" "dir4"

EOF

rm -rf R
echo "$DATA" | darcs convert import R
cd R
[[ -e dir2 && -e dir3 && -e dir4 && (! -e dir) && -e dir2/a && -e dir3/dir4/a
  && -e dir4/a ]]
[[ $(darcs log --count) -eq 2 ]]
