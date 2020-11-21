#!/usr/bin/env bash

# issue2603: clone of a repo with unresolved conflicts gives no warning
# and no applies no conflict markup

. lib

rm -rf R
darcs init R
cd R
echo foo > f
darcs record -lam foo
cd ..

rm -rf S
darcs init S
cd S
echo bar > f
darcs record -lam bar
darcs pull ../R -a --allow-conflicts
cd ..

rm -rf T
darcs clone S T 2>&1 | grep conflicts
