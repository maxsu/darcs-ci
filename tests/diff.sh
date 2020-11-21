#!/usr/bin/env bash
. ./lib

export DARCS_TMPDIR=`pwd`/tmp
rm -rf tmp
mkdir tmp

rm -rf temp1
darcs init temp1
cd temp1

echo text > afile.txt
darcs record -lam init
darcs diff
darcs diff --no-unified -p . --store-in-mem > diffinmem
darcs diff --no-unified -p . --no-store-in-mem > diffondisk
diff diffinmem diffondisk

echo text >> afile.txt
darcs diff | sed 's/afile\.txt.*//'> diffnoarg
darcs diff . | sed 's/afile\.txt.*//' > diffdot
diff diffnoarg diffdot

cd ..
rm -rf temp1


# issue966

darcs init temp1
cd temp1
echo "aaa diff" > file
darcs record -lam "aaa"
echo "bbb diff" >> file
darcs record -a -m "bbb"

darcs diff --patch "aaa" | grep "aaa diff"
darcs diff --patch "bbb" | grep "bbb diff"

darcs tag release-1
darcs optimize clean

echo "ccc diff" >> file
darcs record -a -m "ccc"
darcs diff --patch "ccc" | grep "ccc diff"

# here is where we have a problem
darcs diff --patch "aaa" | grep "aaa diff"
darcs diff --patch "bbb" | grep "bbb diff"
cd ..
rm -rf temp1

# issue1139 diff last

darcs init temp1
cd temp1

echo text > foo
darcs rec -lam 'add foo'

echo newtext > foo
darcs record -am 'modify foo'

darcs diff --no-unified --store-in-mem --last=1 > out1
grep text out1
grep foo out1

darcs diff --no-unified --last=1 > out
grep text out
grep foo out

diff -u out1 out

cd ..
rm -rf temp1

# issue 1139 diff with no args
darcs init temp1
cd temp1

echo text > foo
darcs record -lam 'add foo'
echo newtext > foo
darcs diff --no-unified --store > out1
grep text out1
grep foo out1

darcs diff --no-unified > out
grep text out
grep foo out
diff out out1
cd ..
rm -rf temp1

# issue1290 - darcs diff --index
darcs init temp1
cd temp1

echo '1' > f
darcs record -lam 'one'
echo '2' > f
darcs record -lam 'two'
echo '3' > f
darcs record -lam 'three'
echo '4' > f
darcs record -lam 'four'
# in the following outputs of `darcs diff`, we delete the
# lines "diff -rN old-... new-..." since they can be different
# if tests are run in parallel
darcs diff --no-unified --from-patch one --to-patch two | sed /^diff/d > d1
darcs diff --no-unified --index=3-4 |sed /^diff/d > d2 # the numbers go backwards
diff -q d1 d2
cd ..
rm -rf temp1


# issue2052 - Ensure we use unified Diff by default.

darcs init temp1
cd temp1
touch a
darcs record -lam 'Add a'
echo testing > a
test `darcs diff | grep -c "diff -rN -u"` -eq 1
test `darcs diff --no-unified | grep -c "diff -rN -u"` -eq 0
test `darcs diff --no-unified | grep -c "diff -rN"` -eq 1
cd ..
rm -rf temp1

# issue2067: inexistant files result in empty lines in darcs

darcs init temp1
cd temp1
darcs diff a b c d 2> /dev/null | wc -l | grep "^ *0$"
cd ..
rm -rf temp1

# issue2179 - darcs diff on a dir should diff everything in and below
# that directory

darcs init temp1
cd temp1
mkdir dir
touch dir/file
darcs rec -alm 'Add dir/file'
echo testing > dir/file
darcs wh | grep testing
darcs diff | grep testing
darcs diff dir/file | grep testing
darcs diff dir | grep testing
cd ..
rm -rf temp1
