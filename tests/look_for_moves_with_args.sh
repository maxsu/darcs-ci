#!/usr/bin/env bash

# test that --look-for-moves properly handles file arguments

. ./lib

darcs init R
cd R

touch old1
touch old2
touch old3
darcs record -lam 'added files'
mv old1 new1
mv old2 new2
cd ..

runtest () {
  darcs whatsnew --repodir R --look-for-moves $* | grep move > out
}

num_lines () {
  test $(wc -l < $2) = $1
}

move1='move \./old1 \./new1'
move2='move \./old2 \./new2'

runtest ""
grep "$move1" out
grep "$move2" out
num_lines 2 out

runtest old1
# we expect to see only move1
grep "$move1" out
not grep "$move2" out
num_lines 1 out

runtest old1 old2
# we expect to see both moves
grep "$move1" out
grep "$move2" out
num_lines 2 out

runtest old2 old3
# we expect to see only move2
grep "$move2" out
not grep "$move1" out
num_lines 1 out
