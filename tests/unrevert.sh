#!/usr/bin/env bash
. ./lib

rm -rf temp1
darcs init temp1
cd temp1

echo hello world > foo
darcs record -lam add
echo goodbye world >> foo
cp foo bar
darcs revert -a
darcs show contents foo | cmp foo -

darcs unrevert -a
cmp foo bar

cd ..
rm -rf temp1

# unrevert replace moved

darcs init temp1
cd temp1

echo hello world > foo
darcs record -lam 'addfoo'
darcs replace hello goodbye foo
darcs revert -a

not darcs whatsnew

darcs mv foo bar

echo hello my good friends >> bar

darcs unrevert -a

darcs whatsnew > unrecorded
cat unrecorded

grep 'bar .* hello goodbye' unrecorded

cat bar
grep 'goodbye world' bar
grep 'goodbye my good friends' bar

cd ..
rm -rf temp1

# unrevert cancel
# From issue366 bug report

darcs init temp1
cd temp1

touch a b 
darcs record -lam init
echo plim >> a
echo plim >> b
echo yyyy | darcs revert
echo ploum >> a 
echo nyyy | darcs unrevert

cd ..
rm -rf temp1

# unrevert add

darcs init temp1
cd temp1

echo foo > foo
darcs add foo
darcs whatsnew > correct
cat correct

darcs revert -a

not darcs whatsnew

darcs unrevert -a

darcs whatsnew > unrecorded
cat unrecorded

diff -u correct unrecorded

cd ..
rm -rf temp1

# double unrevert
# This demonstrates a bug that happens if you revert followed by
# a partial unrevert and a full unrevert.  It requires that
# the second unrevert is working with patches who's contents need
# to be modified by the commute in the first unrevert.

darcs init temp1
cd temp1

echo line1 >> A
echo line2 >> A
echo line3 >> A
echo line4 >> A
echo line5 >> A
echo line6 >> A
darcs add A
darcs record -am A
sed 's/line2/Line2/' A  > A1; rm A; mv A1 A
sed '4d' A > A1; rm A; mv A1 A
sed 's/line6/Line6/' A > A1; rm A; mv A1 A
darcs revert -a
echo nyny | darcs unrev
darcs unrev -a

cd ..
rm -rf temp1

# impossible unrevert

darcs init temp1
cd temp1

echo ALL ignore-times > _darcs/prefs/defaults
echo a > foo
darcs record -lam aa
echo b > foo
echo yy | darcs revert -a
echo ydyy | darcs unrecord
# since the unrevert is impossible, we should fail if it succeeds...
echo yy | darcs unrevert && exit 1 || true

cd ..
rm -rf temp1
