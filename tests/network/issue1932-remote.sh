#!/usr/bin/env bash
## Test for issue1932 - "darcs add -qr ." should not break on files with colons
##
## Copyright(C) 2010 Dmitry Astapov
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
. sshlib

# Colons could be in repo names and in file name.
# Colon in repo name is an indication of special case - remote repo.
# Colon in the file could be there under unix and requires no special treatment.

# Repo name with ':' is either scp repo or http repo.
# Let's check scp repo first.
( darcs clone user@invalid:path || true ) > log 2>&1
[ -n "$(grep 'ssh: Could not resolve hostname invalid' log)" ]

# HTTP repo
( http_proxy= darcs clone http://www.bogus.domain.so.it.will.surely.fail.com || true ) 2>&1 | tee log
egrep 'CouldNotResolveHost|host lookup failure|Name or service not known' log

# local repos are tested by tests/issue1932-colon-breaks-add.sh
