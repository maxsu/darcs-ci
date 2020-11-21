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

rm -rf R1 R2

! read -r -d '' DATA <<'EOF'
blob
mark :1
data 0

reset refs/heads/master
commit refs/heads/master
mark :2
author test <test@example.org> 1500000000 +0000
committer test <test@example.org> 1500000000 +0000
data 19
Add files and dirs
M 100644 :1 file
M 100644 :1 foodir/foo1
M 100644 :1 foodir/foo2

commit refs/heads/master
mark :3
author test <test@example.org> 1500000000 +0000
committer test <test@example.org> 1500000000 +0000
data 12
remove foo2
from :2
D foodir/foo2

commit refs/heads/master
mark :4
author test <test@example.org> 1500000000 +0000
committer test <test@example.org> 1500000000 +0000
data 21
Delete foo1 and file
from :3
D foodir/foo1
D file

EOF

echo "$DATA" | darcs convert import R1

darcs init --repo R2
cd R2

echo yydy | darcs pull ../R1

# Ensure we delete the file (but not the directory!)
[[ -d foodir && ! -e foodir/foo2 ]]

darcs pull -a ../R1

# Make sure the folder is now deleted.
[[ ! -d foodir ]]

cd ..

# only continue if git present
git --version | grep -i "git version"   || exit 200

git init gitsource
cd gitsource
mkdir -p dir1/dir2/dir3
echo "i am so deep" > dir1/dir2/dir3/f
mkdir dir4
echo "blabla" > dir4/g
echo "some other file" > h
git add .
git commit -m "blabla"
rm dir1/dir2/dir3/f
git add --all .
git commit -m "deleted f"
rm dir4/g
git add --all .
git commit -m "deleted g"
git clean -fd  # delete dir1 and dir4
cd ..
(cd gitsource  && git fast-export --all) | darcs convert import darcsmirror

function wcDiff() {
   diff --exclude=_darcs --exclude=.git "$1" "$2"
}

wcDiff gitsource darcsmirror
darcs check --repodir=darcsmirror # ensure repo is consistent

# ensure dir is deleted after a file move out of it (a common case)
# and that a dir is created when moving to it (also a common case)

git init gitsource2
cd gitsource2
mkdir -p dir1/
echo "i want to move" > dir1/f
echo "me too!!" > g
echo "12345" > h
echo "67890" > i
git add .
git commit -m "initial commit"
mv dir1/f f
git add --all .
git commit -m "move dir1/f to f"
mkdir dir2
mv g dir2/g
git add --all .
git commit -m "move g to dir2/"
# the following commit could be problematic with darcs import,
# with the addfile hunk ending up between both move hunks.
mkdir dir3
mv h dir3/h
mv i dir3/i
git add --all .
git commit -m "move h and i to dir2/"
git clean -fd
cd ..
(cd gitsource2  && git fast-export --all -M) | darcs convert import darcsmirror2    # -M to generate file moves data

wcDiff gitsource2 darcsmirror2
darcs check --repodir=darcsmirror2 # ensure repo is consistent
