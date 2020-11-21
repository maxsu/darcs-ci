#!/usr/bin/env bash

# This is the scenario described in Appendix B of the camp paper
# to motivate giving identities (there called "names") to prim patches.
# We make the same change A1 and A2 independently in two separate repos,
# then record B that depends on A1. But it doesn't, not really. What it
# depends on is the /change/ made by A1, and since A2 makes the identical
# change we can arrange things so that we replace dependency A1 with the
# alternative dependency A2.

. lib

# this test fails for darcs-1 and darcs-2 repos and we cannot fix it
skip-formats darcs-1 darcs-2

rm -rf R1 R2

darcs init R1
cd R1
echo A > f
darcs record -lam A1 f
echo B > f
darcs record -lam B f
cd ..

darcs init R2
cd R2
echo A > f
darcs record -lam A2 f
darcs pull ../R1 -a --allow-conflicts
# this should fail according to Ian...
darcs obliterate -a -p A1
cd ..

cd R1
# ...instead of this crashing
darcs pull ../R2 -a --allow-conflicts
cd ..

cd R2
# ...or this
darcs pull ../R1 -a --allow-conflicts
cd ..
