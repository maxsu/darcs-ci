#!/usr/bin/env bash
## Test for issue2545 - Argument smuggling in SSH repository URLs
##
## Darcs allows (almost) arbitrary command execution via a crafted ssh
## URI.
## When pushing to a remote repo, darcs is invoked on the remote server
## via ssh. This use of ssh is different from the ones tested by the
## not-networked test. Also, I'm not sure how (if) it can be exploited,
## so I'm just checking for the debug message. Pretty lame test, I know.
##
## Copyright (C) 2017  Gian Piero Carrubba
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
. sshlib                        # Load ssh helpers.

init_remote_repo R              # Create our test repos.
darcs init      --repo R        #
cd R

echo "text" > file              # Modify the working dir
darcs record -lam "First Patch" # Record the changes

check="\"${SSH}\" \"--\" \"${REMOTE}\" \"darcs apply --all --debug --repodir '${REMOTE_DIR}/R'\""
darcs push -a --debug "${REMOTE}":"${REMOTE_DIR}"/R 2>&1 >/dev/null | \
    fgrep "$check"
