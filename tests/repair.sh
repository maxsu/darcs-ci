#!/usr/bin/env bash
. ./lib

rm -rf temp1
darcs init temp1
cd temp1
echo A1 > foo
mkdir d
echo A2 > d/bar
darcs record -lam AA
echo B > foo
darcs record -lam BB
echo C > foo
darcs record -lam CC

for i in _darcs/pristine*; do
  echo Empty the pristine directory: $i
  rm -rf $i
  mkdir $i
done

darcs repair

# remove each hashed pristine file one at a time
for f in $(ls _darcs/pristine.hashed/*); do
  rm $f
  not darcs check
  darcs repair | grep -i "fixing pristine"
done

cd ..

# issue1977: repair complains when there is no pristine.hashed directory

rm -rf temp1
darcs init temp1
cd temp1
echo "a" > a
darcs rec -lam a
rm -rf _darcs/pristine.hashed/
darcs repair
cd ..

# check that repair doesn't do anything to a clean repository

rm -rf temp1
darcs init temp1
cd temp1
touch baz
darcs record -lam moo
darcs repair | grep 'already consistent'
cd ..

# We cannot currently implement repair for the darcs-3 format
# because that risks inconsistencies in conflictors that refer
# to removed or added prims. Fixing this requires refactoring
# the API for patch repair and is therefore postponed.

# START DISABLED TESTS for darcs-3
if test "$format" != darcs-3; then

# test that we can repair incorrect adds

rm -rf temp1
darcs init temp1
cd temp1

echo foo > file
mkdir dir
darcs rec -lam 'initial'

# produce a corrupt addfile patch
echo 'addfile ./file' > _darcs/patches/pending
echo 'yny' | darcs rec -m 're-add file'

not darcs check
darcs repair
darcs check

# produce a corrupt adddir patch
echo 'adddir ./dir' > _darcs/patches/pending
echo 'yy' | darcs rec -m 're-add dir'

not darcs check
darcs repair
darcs check
cd ..

# END DISABLED TESTS for darcs-3
fi

# These tests should be fixed but this is hard, since 'darcs record'
# no longer allows recording patches with invalid file paths.

# START DISABLED TESTS
if false; then

# test for repair of a corrupt repository 

rm -rf temp1
darcs init temp1
cd temp1

echo foo > bar
darcs rec -lam 'foo'

echo hey > foo
darcs rec -lam 'more foo'

hashed=false
test -e _darcs/hashed_inventory && hashed=true
cp -R _darcs _clean_darcs

# produce a corrupt patch
echo 'rmfile foo' > _darcs/patches/pending
$hashed || echo -n > _darcs/pristine/foo
darcs rec -a -m 'remove foo'

not darcs check # unapplicable patch!
cp -R _darcs/ _backup_darcs
darcs repair # repairs the patch
darcs check
rm -rf _darcs
mv _backup_darcs _darcs # get the bad patch back

# stash away contents of _darcs
cp -R _darcs/ _backup_darcs

echo here > bar
darcs rec -a -m 'here'

# corrupt pristine content
corrupt_pristine() {
    $hashed && inv=`grep ^pristine _darcs/hashed_inventory`
    cp _backup_darcs/patches/* _darcs/patches/
    cp _backup_darcs/*inventory* _darcs/
    if $hashed; then
        cp _darcs/hashed_inventory hashed.tmp
        sed -e "s,^pristine:.*$,$inv," < hashed.tmp > _darcs/hashed_inventory
        rm hashed.tmp
    fi
}

corrupt_pristine
not darcs check # just a little paranoia

darcs repair # repair succeeds
darcs check # and the resulting repo is consistent

# *AND* it contains what we expect...
darcs show contents bar > foobar
echo foo > foobar1
diff foobar foobar1

rm -rf _backup_darcs
mv _clean_darcs _backup_darcs
corrupt_pristine # without the unapplicable patch
not darcs check
darcs repair
darcs check

cd ..

# END DISABLED TESTS
fi

# issue2001: check (alias for repair --dry-run) is not read-only 

rm -rf temp1
darcs init temp1
cd temp1
mkdir d e
echo 'Example content.' > d/f
darcs record -lam 'Add d/f and e.'
darcs mv d/f e/
darcs record -am 'Move d/f to e/f.'
rm _darcs/pristine.hashed/*     # Make the repository bogus
cp -r _darcs archive
not darcs check
diff -r _darcs archive
cd ..
