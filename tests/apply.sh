#!/usr/bin/env bash
## Test for issue2017 - apply should gracefully handle tag missing
## from context (complain, not crash)
##
## Copyright (C) 2010 Eric Kow
## Copyright (C) 2012 Owen Stephens
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib                           # Load some portability helpers.

# issue1427: apply gzipped bundles

rm -rf temp1 temp2
darcs init temp1
darcs init temp2

cd temp1
touch foo bar
darcs record -lam add_foo_bar
darcs mv foo zig
darcs mv bar foo
darcs mv zig bar
darcs record -lam swap_foo_bar
darcs send --output=funpatch --dont-sign -a ../temp2

gzip funpatch

cd ../temp2
darcs apply ../temp1/funpatch.gz
cd ..
cmp temp1/bar temp2/bar

rm -rf temp2
darcs init temp2
cd temp2
darcs apply ../temp1/funpatch.gz
## Also test that "darcs apply" can accept a patch on stdin.
darcs obl -a
darcs apply < ../temp1/funpatch.gz
cd ..
cmp temp1/bar temp2/bar

## issue2017 - apply should gracefully handle tag missing
## from context (complain, not crash)

rm -rf R* S* T*
darcs init R
cd R
echo 'Example content.' > f
darcs record -lam 'Add f'
cd ..

# variant 0 - this passes trivially
darcs clone R R0
darcs clone R0 S0
darcs tag 's' --repo S0
darcs clone S0 T0
cd T0
echo 'More content.' > f
darcs record -lam 'Modify f'
darcs send -o foo.dpatch -a
cd ..
not darcs apply --repo R0 T0/foo.dpatch > log 2>&1
not grep bug log
grep "Cannot find tag" log

# variant 1 - tag in shared context
darcs clone R R1
darcs tag '1' --repo R1
darcs clone R1 S1
darcs tag 's1' --repo S1
darcs clone S1 T1
cd T1
echo 'More content.' > f
darcs record -lam 'Modify f'
darcs send -o foo.dpatch -a
cd ..
# sanity check: should be able to cherry pick
darcs clone R1 R1b
cd R1b
[ `darcs log --count` -eq 2 ]
darcs pull ../T1 --match 'touch f' --all
[ `darcs log --count` -eq 3 ]
cd ..
# the test: can't apply this due to incorrect context
not darcs apply --repo R1 T1/foo.dpatch > log 2>&1
not grep 'bug' log
grep "Cannot find tag" log

# variant 2 - tag created after the fact
darcs clone R  R2
darcs clone R2 S2
darcs tag 's2' --repo S2
darcs clone S2 T2
cd T2
echo 'More content.' > f
darcs record -lam 'Modify f'
darcs send -o foo.dpatch -a
cd ..
darcs tag '2'  --repo R2  # only tag after
not darcs apply --repo R2 T2/foo.dpatch > log 2>&1
not grep 'bug' log
grep "Cannot find tag" log

# issue1921
# Attempting to apply a patch which depends on a missing tag should not cause
# darcs to die.

rm -rf R* S* T*
darcs init R
cd R

# Setup a repo with a tagged patch, and another patch ontop, so we have a split
# inventory
touch file1
darcs rec -alm 'Add file1'
darcs tag -m 'file1 tag'
touch file2
darcs rec -alm 'Add file2'

# Take a copy of the repo at this point
darcs clone . ../S

# Add the tag which we will fail on
darcs tag -m 'file2 tag'

# Take a copy with the tag
darcs clone . ../T

# Add our patch which will depend only on the last tag.
echo 'file1' > file1
darcs rec -am 'file1 content'

# Create a patch bundle with the new patch (by sending against the repo we
# copied, with the last tag)
darcs send ../T -a -o ../patch.dpatch --no-edit-description

cd ../S

# Try to apply to the patch which depends on the missing tag (we expect darcs
# to fail gracefully here)
not darcs apply ../patch.dpatch &> apply_output.txt

# A best-attempt at ensuring darcs warns about the missing tag:
grep "Cannot find tag file2" apply_output.txt
cd ..
rm -rf R S

## issue1873 - apply should complain about the right
## patches if it says some are missing

rm -rf R S
darcs init R
cd R
echo a > a
darcs rec -lam a
echo b > a
darcs rec -lam b
echo x > x
darcs rec -lam x
echo c > a
darcs rec -lam c
echo y > y
darcs rec -lam y
echo d > a
darcs rec -lam d
cd ..

darcs clone R S
darcs unpull -p x -a --repo R
darcs send   --no-minimize -p x -a --repo S -o R/x.dpatch
darcs unpull -p y -a --repo R
not darcs apply --repo R R/x.dpatch 2>&1 | tee log

not grep '^  \* d' log # does not complain about an unrelated patch
    grep '^  \* y' log # complains about the offending one instead

## Test that apply --skip-conflicts filters the conflicts
## appropriately.

rm -rf R S
darcs init R
cd R
echo 'foo' > foo
echo 'bar' > bar
darcs rec -lam 'Add foo and bar'
darcs clone . ../S
echo 'foo2' > foo
darcs rec -lam 'Change foo (2)'
echo 'bar2' > bar
darcs rec -lam 'Change bar (2)'
cd ../S
echo 'foo3' > foo
darcs rec -lam 'Change foo (3)'
cd ../R
darcs send -a ../S -o ../S/applyme.dpatch
cd ../S
darcs apply --skip-conflicts applyme.dpatch
test `darcs log --count` -eq 3
cd ..

# issue2193 - "darcs apply --test runs the test twice.

rm -rf R S
darcs init R
darcs clone R S

# Create a patch bundle
cd R
echo 'Example content.' >file1
darcs rec -lam patch1
darcs send --dont-edit-description --output=./patch1 -a ../S

# Setup a test that prints a unique string, apply the patch set,
# check that the unique string occurs in the output once.
cd ../S
darcs setpref test 'echo 2a427e65f322be754dce67c829e5f8a3'
darcs apply --test ../R/patch1 > log 2>&1
[ `fgrep -c 2a427e65f322be754dce67c829e5f8a3 log` -eq 1 ]
