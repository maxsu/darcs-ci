#!/usr/bin/env bash

# Test that repositories that contain legacy "UNDO" patches, i.e. ones with
# an inverted PatchInfo, are still handled correctly by darcs. These
# patches were written by darcs rollback until the behaviour of rollback
# was changed in 2008; one example of a repo containing them is darcs itself.

. ./lib

rm -rf undo

# undo is a repository containing a legacy "UNDO" patch,
# i.e. one with an inverted PatchInfo.
#
# As darcs hasn't created these  patches for many years, it's not easy to
# build such a repo. This one was created using rebase on the actual darcs
# repo. As a result the UNDO patch isn't actually an inverse of the original
# patch, as its PatchInfo got rewritten during unsuspend. This shouldn't
# matter in practice for this test.

unpack_testdata undo

cd undo

darcs changes | grep "UNDO"
darcs changes --xml | grep "inverted='True'"

echo "empty" > Patch.lhs

echo yy | darcs amend -a -p "get rid of"

darcs changes | not grep "UNDO"
darcs changes --xml | not grep "inverted='True'"
