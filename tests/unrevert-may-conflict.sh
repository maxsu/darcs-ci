#!/usr/bin/env bash

# 'darcs unrevert' may result in a conflict: revert a change,
# record another change that conflicts with what was reverted,
# then unrevert.
# This was originally thought to be a bug (issue2609) but really
# is normal, expected behavior.

. lib

rm -rf R

darcs init R
cd R
echo A > f
darcs record -lam A f
echo V > f
darcs revert -a
echo B > f
darcs record -lam B f
darcs unrevert -a 2>&1 | tee unrevert_log
grep conflicts unrevert_log
cd ..
