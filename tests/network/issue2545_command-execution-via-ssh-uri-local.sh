#!/usr/bin/env bash
## Test for issue2545 - Argument smuggling in SSH repository URLs
## Darcs allows (almost) arbitrary command execution via a crafted ssh URI.
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

# This part of the tests for issue2545 doesn't actually try to connect
# remotely, but is still kept in the network folder as it uses a
# network-based command, allowing developers that have problems with
# those commands to exclude it with --network=no:
# https://lists.osuosl.org/pipermail/darcs-devel/2020-July/021363.html

. lib                           # Load some portability helpers.
darcs init      --repo R        # Create our test repos.
cd R

DARCS_SCP=sftp darcs pull -a ssh://-oProxyCommand='touch FAIL' \
    2>/dev/null || true
not ls FAIL >/dev/null

DARCS_SCP=sftp darcs pull -a -- -oProxyCommand='touch FAIL':dir \
    2>/dev/null || true
not ls FAIL >/dev/null

# Executing the same tests with `clone' instead of `pull'. The results shoud
# be the same, but better safe than sorry.
DARCS_SCP=sftp darcs clone ssh://-oProxyCommand='touch FAIL' S \
    2>/dev/null || true
not ls FAIL >/dev/null

DARCS_SCP=sftp darcs clone -- -oProxyCommand='touch FAIL':dir T \
    2>/dev/null || true
not ls FAIL >/dev/null

cd ..
