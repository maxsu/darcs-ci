#!/usr/bin/env bash

. lib

rm -rf temp1 temp2
darcs init temp1
cd temp1
touch t.t
darcs record -lam "initial add"
darcs log --context > my_context
DIR=`pwd`
abs_to_context="${DIR}/my_context"
cd ..

darcs clone temp1 --context="${abs_to_context}" temp2
darcs log --context --repo temp2 > repo2_context
diff -u "${abs_to_context}" repo2_context

# issue1865: cover interaction of clone --context with tags

rm -rf temp1 temp2
darcs init temp1
cd temp1
touch t.t
darcs record -lam "initial add"
darcs tag -m tt
echo x > x
darcs rec -lam "x"
darcs log --context > my_context
abs_to_context="$(pwd)/my_context"
cd ..

darcs clone temp1 --context="${abs_to_context}" temp2
darcs log --context --repo temp2 > repo2_context
diff -u "${abs_to_context}" repo2_context

# issue1041

rm -rf temp1 temp2
# should fail, since temp1 doesn't exist
not darcs clone temp1 temp2
# verify that temp2 wasn't created
not cd temp2

# issue2199 "darcs clone --tag" gets too much if tag is dirty

rm -rf temp1 temp2 temp3
darcs init temp1
cd temp1
echo 'wibble' > file
darcs rec -lam 'wibble'
echo 'wobble' > file
darcs rec -lam 'wobble'
cd ..

darcs clone temp1 temp2

cd temp2
darcs unpull --patch 'wobble' -a
darcs tag 'wibble'
cd ..

cd temp1
darcs pull ../temp2 -a
cd ..

darcs clone --tag wibble temp1 temp3
cd temp3
darcs log | not grep wobble
cd ..

# issue885: darcs clone --to-match

rm -rf temp1 temp2 temp3
darcs init temp1
cd temp1
echo first > a
darcs record -lam 'first'
firsthash=`darcs log --xml | grep 'hash=' | sed -e "s/.*hash='//" -e "s/'>//"`
echo second > b
darcs record -lam 'second'
cd ..

darcs clone --to-match "hash $firsthash" temp1 temp2
test $(darcs log --count --repodir temp2) -eq 1

darcs clone --to-hash $firsthash temp1 temp3
test $(darcs log --count --repodir temp3) -eq 1

# various tests for clone --tag

rm -rf temp1 temp2
darcs init temp1
cd temp1
echo ALL ignore-times >> _darcs/prefs/defaults
echo A > foo
darcs record -lam AA
echo B > foo
darcs record -am BB
echo C > foo
darcs record -am CC
darcs tag -m 1.0
cp foo foo_version_1.0
echo D > foo
darcs record -am DD
echo E > foo
darcs record -am EE
echo F > foo
darcs record -am FF
cd ..

darcs clone --tag 1.0 --repo-name temp2 temp1
cmp temp2/foo temp1/foo_version_1.0

# clone --tag with commuted patches

rm -rf temp1 temp2 temp3
darcs init temp1
cd temp1
cat > file <<EOF
1
2
3
4
EOF
darcs rec -alm 'Add file'
cat > file <<EOF
2
3
4
EOF
darcs rec -alm 'Remove line 1'
cat > file <<EOF
2
4
EOF
darcs rec -alm 'Remove line 3'
cd ..

darcs init temp2
cd temp2
echo ynyy | darcs pull ../temp1
darcs tag -m Tag
darcs push -a ../temp1
cd ..

darcs clone --tag=Tag temp1 temp3
cd temp3
darcs check
cd ..

# clone --tag : check that pending looks ok

rm -rf temp1 temp2
darcs init temp1
cd temp1
mkdir d
darcs rec -lam 'add d'
darcs tag t
rmdir d
darcs rec -am 'rm d'
cd ..

darcs clone --tag t temp1 temp2
cd temp2
if [ -f _darcs/patches/pending ]; then
	if grep -v '^[{}]$' _darcs/patches/pending >/dev/null; then
		cat _darcs/patches/pending
		exit 1
	fi
fi
cd ..

# issue2230 - darcs clone --context checks the validity of the context
# file too late.

rm -rf temp1 temp2
darcs init temp1
touch fake_context.txt
not darcs clone --context fake_context.txt temp1 temp2

# The clone should fail, so we shouldn't have an temp2 repo
[[ ! -e temp2 ]]
