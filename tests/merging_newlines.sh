#!/usr/bin/env bash

# trick: requiring something to fail
. lib

# A test for darcs detecting a conflict, inspired by bug #152 in RT

# set up the repository
darcs init temp1

cd temp1
echo "apply allow-conflicts" > _darcs/prefs/defaults
echo "from temp1" > one.txt
darcs add one.txt
darcs record -am "add one.txt"
echo >> one.txt
darcs wh -u
cd ..

darcs get temp1 temp2
cd temp2
echo "from temp2" >> one.txt
darcs whatsnew -s | grep M
darcs record -am "append non-empty line"
darcs push -av 2> log
grep -i conflicts log
cd ..
