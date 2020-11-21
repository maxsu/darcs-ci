#!/usr/bin/env bash
. ./lib

# Some tests for 'darcs log -a'

rm -rf temp1
darcs init temp1
cd temp1

date >> date.t
darcs record -A 'Mark Stosberg <a@b.com>' -lam foo

####

darcs log date.t > out # trivial case first
cat out
grep foo out

darcs log --last=1 date.t > out
cat out
grep foo out

darcs log --last 1 --summary date.t > out
cat out
grep foo out

darcs log --last=1 --xml > out
cat out
grep '&lt;a@b.com&gt;' out # check that --xml encodes < and >

###

# Add 6 records and try again
for i in 0 1 2 3 4 5; do
    date >> date.t
    darcs record -a -m "foo record num $i" date.t
done

darcs log date.t > out
cat out
grep foo out

darcs log --last=1 date.t > out
cat out
grep foo out

darcs log --last 1 --summary date.t > out
cat out
grep foo out

###

darcs log --context --from-patch='num 1' --to-patch 'num 4' > out
cat out
grep 'num 4' out
grep 'num 3' out
grep 'num 2' out
grep 'num 1' out

cd ..


# Some tests for the output of log when combined with move.

rm -rf temp2
darcs init temp2
cd temp2
date > foo
darcs record -lam 'add foo'
mkdir d
darcs record -lam 'add d'
darcs mv foo d
darcs record -m 'mv foo to d' -a
darcs mv d directory
darcs record -m 'mv d to directory' -a
echo 'How beauteous mankind is' > directory/foo
darcs record -m 'modify directory/foo' -a
darcs log directory/foo > log
grep 'add foo' log
grep 'mv foo to d' log
echo 'O brave new world' > directory/foo
# darcs should also take unrecorded moves into account
darcs mv directory/foo directory/bar
darcs log directory/foo > log
grep 'mv foo to d' log
echo 'That has such people in it' > directory/foo
darcs add directory/foo
darcs record -m 'mv foo then add new foo' -a
darcs annotate directory/bar | tee log
grep 'O brave new world' log
grep "mv foo then add new foo" log
not grep "unknown" log
cd ..

# Issue244
# darcs changes should be able to pull up the history for a file
#   using its moved and not-yet recorded new name

rm -rf temp3
darcs init temp3
cd temp3
touch b
darcs record -lam 11
darcs mv b c
darcs log c | grep 11
cd ..

## issue1337 - darcs log shows unrelated patches
## Asking "darcs log" about an unrecorded file d/f will list the
## patch that creates the parent directory d/ (instead of no patches).

rm -rf temp4
darcs init temp4
cd temp4
mkdir d
darcs record -lam d d
# We use --match 'touch d/f' instead of simply d/f because the latter
# prints "Changes to d/f:\n" before the count.
test 0 -eq "$(darcs log --count --match 'touch d/f')"

cd ..

## issue1632 - 'darcs changes d/f' should not list any changes,
## where d is part of the repo and f is a non-existent file.

rm -rf temp5
darcs init temp5
cd temp5

mkdir d
darcs record -lam 'added directory d'
# darcs should not list any changes here:
darcs changes non-existent-file > log
not grep 'added directory d' log
# ...and neither here:
darcs changes d/non-existent-file > log
not grep 'added directory d' log
cd ..

## issue1888 - changes --context is broken when topmost patch
## is a clean tag.

rm -rf temp6
darcs init temp6
cd temp6

echo a > a ; darcs rec -lam "patch_a"
darcs log --context | grep patch_a

darcs tag -m "tag_a"
darcs log --context | not grep patch_a
darcs log --context | grep tag_a

echo b > a; darcs rec -lam "patch_b"
darcs log --context | not grep patch_a
darcs log --context | grep tag_a
darcs log --context | grep patch_b
cd ..

## log --index=N-M

# re-use the temp6 directory with 3 patches
cd temp6
count=$(darcs log --count)
for N in $(seq 1 $count); do
  for M in $(seq 1 $count); do
    darcs log --index=$N-$M --count | tee LOG
    expected=$(($N <= $M ? $M - $N + 1 : 0))
    grep -w $expected LOG
  done
done
cd ..
