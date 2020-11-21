#!/usr/bin/env bash

# test that 'darcs show dependencies' shows only the direct dependencies

. lib

rm -rf R
darcs init R
cd R

# 4 patches, each depending on the previous one
echo a > f
darcs record -lam a
echo b > f
darcs record -lam a
echo c > f
darcs record -lam a
echo d > f
darcs record -lam d

# the number of edges in the dependency graph should be exactly 3
darcs show dependencies > graph.dot
test "3" = $(grep -wF -- '->' graph.dot | wc -l)

cd ..
