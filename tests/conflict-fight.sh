#!/usr/bin/env bash

. ./lib

# step 1
mkdir temp0
cd temp0
darcs init
echo m1 > foo
darcs add foo
darcs record -a -m m1
cd ..

# step 2
darcs get temp0 temp1
cd temp1
echo a1 > foo
darcs record foo -a -m a1
cd ..


# step 3
cd temp0
echo m2 > foo
darcs record -a -m m2
cd ..


# step 4
cd temp1
darcs pull -a
echo m2-a1 > foo
darcs record -a -m 'Fix conflict m2-a1'
echo a2 > foo
darcs record -a -m a2
cd ..

#step 5
cd temp0
echo m3 > foo
darcs record -a -m m3
cd ..

#step 6
darcs get temp0 temp2
cd temp2
echo temp2 > _darcs/prefs/author
echo b1 > foo
darcs record -a -m b1

cd ..

#step 7
cd temp0
echo m4 > foo
darcs record -a -m m4
cd ..

#step 8
cd temp1
darcs pull -a
echo m2-a1-m4 > foo
darcs record -a -m 'Fix three-way m2/m2-a1/m4'
echo a3 > foo
darcs record -a -m a3
cd ..

#step 9
cd temp1
darcs pull -av ../temp2
cd ..
