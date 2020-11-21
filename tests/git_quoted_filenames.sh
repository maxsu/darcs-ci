#!/usr/bin/env bash
## Test for issue2489 - convert import/export with spaces in paths
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

. lib                           # Load some portability helpers.

# test http://bugs.darcs.net/issue2489 :
# correctly import quoted paths

function createFiles () {
    mkdir "i have spaces"
    echo "some contents" > "i have spaces/me too"
    echo "other contents" > "more spaces please"
    newline=$(echo -e 'a\nb')
    quoted='"quoted'
    echo testing1 > "$newline"
    echo testing2 > $quoted
}

function wcDiff() {
   diff --exclude=_darcs --exclude=.git "$1" "$2"
}

commitMsg="some files and dirs that need quoting"

# Filenames containing double quotes are forbidden in Windows 10
# Although they can be written using raw NTFS APIs, it's probably
# not worth trying to support them. IO operations in the standard
# Haskell library don't support them either.
# https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
abort_windows

# only run if git present
git --version | grep -i "git version"   || exit 200

rm -rf gitsource gitmirror darcssource darcsmirror

git init gitsource
cd gitsource

createFiles
git add .
git commit -m "$commitMsg"

cd ..

(cd gitsource && git fast-export --all) > gitexport
darcs convert import darcsmirror < gitexport

# working copies should be the same
wcDiff gitsource darcsmirror

# darcs to git direction

darcs init darcssource
cd darcssource
createFiles

# TODO: check this for testing on windows.
darcs add --reserved-ok $quoted

darcs rec -lam "$commitMsg"

git init ../gitmirror
darcs convert export > ../darcsexport
(cd ../gitmirror && git fast-import && git checkout) < ../darcsexport
cd ..

# working copies should be the same
wcDiff darcssource gitmirror
