#!/usr/bin/env bash

# Issue595
#
# A test for running "darcs get" when the parent directory has restrictive
# permissions.  The bug is that darcs trys to "chdir" to the current directory
# using the full path.  The permissions on the parent directory prevent this
# from working, even though the current repo and the remote have sufficient
# permissions.
#
# The real-world case where this would happen would be a web-server with
# restrictive permissions on "/home", with a user running darcs within that.

. lib

abort_windows

rm -rf tmp_remote tmp_restrictive

# Set up a "remote" repo
darcs init tmp_remote

DIR=`/bin/pwd`
trap "chmod +wx $PWD/tmp_restrictive" EXIT

# Set up a directory with restrictive permissions
mkdir -p tmp_restrictive/liberal
cd tmp_restrictive/liberal
chmod a-wx ../../tmp_restrictive
# sanity check that we can not cd ..
not cd ..
# sanity check that we can cd out and back
(cd ../.. && cd tmp_restrictive/liberal) || exit 200
(touch can_touch && test -e can_touch) || exit 200
# now run the real test
darcs get "$DIR/tmp_remote" 2>&1 >log
not grep -i 'permission denied' log
cd ../..
