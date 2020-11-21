#!/usr/bin/env bash

# Check that the latest darcs can still use the patch index
# produced by released versions.

. lib

# A tarball of a repo with a v2 patch index, which at the
# time of writing was the version in use in the last released
# version of darcs.

unpack_testdata patch-index-v2

cd pi

# If darcs can't read the index but thinks it can, this will crash.
# If there's a new patch index format and the format version has
# been properly bumped, then darcs will simply delete and replace it
# and this command will work.
darcs annotate file1.txt

# It would be nice to check that the released version of darcs also
# correctly handles any changes we make, but it's much harder to write
# a test that relies on the released version.

# When a change is made to the patch index format, the
# format version should be bumped and a new sample repo should be
# added to this test, to ensure that the new format is properly
# covered by the tests as soon as it is released.

# The "released" version above should be kept to maintain the
# guard against regressions.

# If further changes to the format are made before the next release,
# it should be ok to keep the same (new) version, though it might
# mean developers/people using the bleeding edge see some breakage.

# Once the release happens, it should be ok to delete the test for
# the old format, on the assumption we are unlikely to make a gross
# mistake like decrementing the format version.
