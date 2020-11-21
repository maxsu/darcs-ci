#!/usr/bin/env bash
. ./lib


# issue701

# step 1
rm -rf temp0
darcs init temp0
cd temp0
echo m1 > foo
darcs record -lam m1
cd ..

# step 2
rm -rf temp1
darcs clone temp0 temp1
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
cd ..

# step 5
cd temp0
echo m3 > foo
darcs record -a -m m3
cd ..

# step 6
cd temp0
echo m4 > foo
darcs record -a -m m4
cd ..

# step 7
cd temp1
darcs pull -a
echo m2-a1-m4 > foo
echo y | darcs mark-conflicts
cd ..
