#!/usr/bin/env bash

. lib

# Some tests for 'darcs whatsnew '

rm -rf temp1

mkdir temp1
cd temp1

# RT#505 whatsnew -s after removal of file without a newline
darcs init
echo -n foobar > foo
darcs record -la -m "add foo" | grep "Finished record"
rm -f foo
darcs whatsnew -s | grep R
darcs record -a -m "remove foo"

# RT#245 --look-for-adds implies --summary
touch look_summary.txt
darcs whatsnew -l | grep -i "a ./look_summary.txt"

# whatsnew works with uncommon file names and does NOT display
# the internal "white space encoded" filename
if echo $OS | grep -i windows; then
  echo  test does not work on windows
  exit 0;
else
  echo foo > \\
  darcs add \\
  darcs whatsnew | tee log
  grep 'hunk ./\\' log
fi

echo foo > "foo bar"
darcs add "foo bar"
darcs wh | tee log
grep 'hunk ./foo bar' log

# check that filename encoding does not botch up the index
darcs rec -am "weird filenames"
not darcs wh

# whatsnew works with absolute paths
DIR="`pwd`"
echo date.t > date.t
touch date.t
darcs add date.t
darcs whatsnew "$DIR/date.t" | grep hunk

cd ..

rm -rf temp1


## Part 2
## This tests the basic fascilities of `whatsnew --interactive`
## Copyright (C) 2014 Daniil Frumin

rm -rf wn-i

darcs init --repo wn-i
cd wn-i

echo lolz > foo
darcs add foo

echo n | darcs whatsnew -i > what
grep "Will not ask whether to view 1" what
rm what

echo yxgq | darcs whatsnew -i > what2
grep "M ./foo +1" what2
addfileCount=`grep -c "addfile" what2`
if [ "$addfileCount" -ne 3 ]; then
    exit 1
fi;
cd ..

## Part 3
## Ensure that darcs whatsnew <paths> only lists relevant bits.
## Public Domain, 2010, Petr Rockai

rm -rf R                        # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.

cd R
mkdir d e                       # Change the working tree.
echo 'Example content.' > d/f
darcs record -lam 'Add d/f and e.'
darcs remove d/f
not darcs wh e # | not grep f
cd ..

## Part 4
# Some tests for 'darcs whatsnew '

rm -rf temp1

darcs init temp1
cd temp1

date > foo
mkdir bar
echo hello world > bar/baz

darcs record -la -m "add foo"

echo goodbye world >> bar/baz

# goodbye should show up precisely once

darcs wh > out
cat out
grep goodbye out | wc -l | grep 1

darcs wh bar bar/baz > out
cat out
grep goodbye out | wc -l | grep 1

darcs mv foo bar
echo not date > bar/foo

darcs wh bar bar/baz > out
cat out
grep date out | wc -l | grep 1

darcs wh foo > out
cat out
grep date out | wc -l | grep 1

darcs wh foo foo foo > out
cat out
grep date out | wc -l | grep 1

darcs wh foo ./foo ../temp1/foo > out
cat out
grep date out | wc -l | grep 1

darcs wh foo bar/../foo > out
cat out
grep date out | wc -l | grep 1

darcs wh foo foo/../foo/. > out
cat out
grep date out | wc -l | grep 1

cd ..
rm -rf temp1

## Part 5

darcs init temp1
cd temp1
touch foo
darcs add foo
darcs rec -m t1 -a -A tester
echo 1 >> foo
darcs what -s | grep -v No\ changes
darcs what -l | grep -v No\ changes
darcs what -sl | grep -v No\ changes

cd ..
rm -rf temp1

