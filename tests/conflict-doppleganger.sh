#!/usr/bin/env bash

. lib

# Tests for the doppleganger conflict bug.

# For Zooko, with love
# Also, for issue81.

check_conflict() {
cat out
if test "$format" = darcs-2; then
    not grep 'conflicts' out
else
    grep 'conflicts' out
fi
}

# check doppleganger conflicts
rm -rf tmp_dopple tmp_ganger
mkdir tmp_dopple
cd tmp_dopple
darcs init

touch a.txt
darcs add a.txt
darcs record -am 'adding a.txt'
cd ..

darcs get tmp_dopple tmp_ganger

for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    cd $repo
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple 2> out
check_conflict
cd ..

# Checking resolution dopplegangers conflicts
rm -rf temp0 temp1 temp2 tmp_dopple tmp_ganger
mkdir temp0
cd temp0
darcs init
touch a.txt
darcs record -la a.txt -m init
cd ..

# Create a conflict
darcs get temp0 temp1
cd temp1
darcs show repo
echo temp1 > a.txt
darcs record -am temp1
cd ..
darcs get temp0 temp2
cd temp2
echo temp2 > a.txt
darcs record -am temp2
cd ..

# Resolve the conflict the same way on both sides
for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    darcs get temp1 $repo
    cd $repo
    darcs pull -a ../temp2
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple 2> out
check_conflict
