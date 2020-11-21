#!/usr/bin/env bash
. ./lib

# unrecord remove

rm -rf temp1
darcs init temp1
cd temp1

echo foo > foo
darcs record -lam 'addfoo'
darcs remove foo
darcs whatsnew > correct
darcs record -a -m 'rmfoo'
darcs unrecord -a --last 1
darcs whatsnew > unrecorded
diff -u correct unrecorded

cd ..
rm -rf temp1

# unrecord setpref

darcs init temp1
cd temp1

darcs setpref boringfile foobar

darcs whatsnew > correct
cat correct

darcs record -a -m 'boringfoobar'
darcs unrecord -a

darcs whatsnew > unrecorded
cat unrecorded

diff -u correct unrecorded

cd ..
rm -rf temp1

# unrecord add

darcs init temp1
cd temp1

echo foo > foo
darcs add foo
darcs whatsnew > correct
cat correct

darcs record -a -m 'addfoo'

darcs unrecord -a

darcs whatsnew > unrecorded
cat unrecorded

diff -u correct unrecorded

cd ..
rm -rf temp1

# tricky unrecord

darcs init temp1
cd temp1

date > temp.c
darcs record -lam hi

mkdir d
darcs add d
darcs mv temp.c d/
darcs record -am mvetc
darcs show contents d/temp.c | cmp d/temp.c -

echo y/d/y | tr / \\012 | darcs unrecord
darcs whatsnew

darcs record -a -m again
darcs show contents d/temp.c | cmp d/temp.c -

cd ..
rm -rf temp1

# Check that the right patches get unrecorded using --dont-prompt-for-dependencies

darcs init temp1
cd temp1

echo foo > f
darcs record -Ax -alm foo1
echo bar > b
darcs rec -Ax -alm bar1
echo foo2 > f
darcs record -Ax -alm foo2
echo bar2 > b
darcs record -Ax -alm bar2
darcs unrec --no-deps -p foo1
darcs changes -p foo --count | grep 2
#foo1 is depended upon, we don't unpull it
echo yy | darcs unrec --dont-prompt-for-dependencies -p foo1
#on the previous line, we don't get asked about foo2.
darcs changes -p foo --count | grep 0
#yet, it is unrecorded.
darcs changes -p bar --count | grep 2
cd ..
rm -rf temp1


# issue1012: rm/record/unrecord/record => inconsistent repository

darcs init temp1
cd temp1

echo temp1 >File.hs
darcs add File.hs
darcs record File.hs -a -m "add File"
rm File.hs
darcs record -a -m "rm File"
darcs unrecord -p "rm File" -a
darcs record -a -m "re-rm File"
cd ..
rm -rf temp1
