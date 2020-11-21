#!/usr/bin/env bash
. ./lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo hello world > foo
darcs add foo
darcs record -lam add
echo goodbye world >> foo
darcs revert -a
darcs show contents foo | cmp foo -

# Now let's test a trickier revert where changes commute nontrivially.

cat > foo <<EOF
a
b
c
d
e
EOF

darcs record -am cleanup

mv foo foo.tmp
cat foo.tmp | grep -v b | grep -v d > foo

echo "nyy" | darcs revert

DARCS_DONT_COLOR=1 darcs wh > whatsnew
cat > correct <<EOF
hunk ./foo 2
-b
EOF
diff -c correct whatsnew

# Try a situation where earlier (kept) changes are depended upon by the
# changes we want to revert:

darcs record -am cleanup

echo hello world > bar
echo hello world > foo
darcs add bar
darcs replace hello goodbye bar foo

echo "cnnnyy/y" | tr / \\012 | darcs revert

DARCS_DONT_COLOR=1 darcs wh > whatsnew
cat > correct <<EOF
addfile ./bar
hunk ./bar 1
+goodbye world
hunk ./foo 1
-a
-c
-d
-e
+hello world
EOF
diff -c correct whatsnew

cd ..
rm -rf temp1

# test for reverting removed directory

darcs init temp1
cd temp1
mkdir d
darcs add d
darcs record -lam 'add dir'
rmdir d
darcs revert -a d
cd ..
rm -rf temp1

# revert unrecorded add

darcs init temp1
cd temp1
echo stuff > foo
darcs add foo
darcs revert -a

