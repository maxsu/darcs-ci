#!/usr/bin/env bash

# Testing amend

. lib

rm -rf temp1
# set up the repository
mkdir temp1
cd temp1
darcs init
cd ..

# do some work here
cd temp1
# Plain amend
touch foo
darcs add foo
darcs record -a -m add_foo
echo 'another line' > foo
echo y | darcs amend -a foo | grep -i 'amending changes'
darcs changes -v | grep 'another line'
# amend of removed file
touch bar1
touch bar2
cat > bar1 << FOO
a line
b line
FOO
darcs add bar1 bar2
darcs record -a -m add_bars
rm -f bar2
echo y | darcs amend -a | grep -i 'finished amending'
# Special case: patch is empty after amend
cp foo foo.old
echo 'another line' >> foo
darcs record -a -m add_line foo | grep -i 'finished recording'
mv foo.old foo
echo y | darcs amend -a foo | grep -i 'amending changes'
# Amend --author, -m, etc
echo "another line" >> foo
echo y | darcs amend -a -m new_name foo | grep -i 'amending changes'
darcs changes --last=1 | grep new_name
echo "another line" >> foo
echo y | darcs amend -a -m new_name -A new_author foo | grep -i 'amending changes'
darcs changes --last=1 | grep new_author

# check that normally the date changes when we amend
echo "another line" >> foo
darcs changes --last=1 | head -n 1 > old_date
sleep 1
echo y | darcs amend -a foo -A new_author | grep -i 'amending changes'
darcs changes --last=1 | head -n 1 > new_date
not cmp old_date new_date

# check that --keep-date works
echo "another line" >> foo
darcs changes --last=1 | head -n 3 | grep Date > old_date
sleep 1
echo y | darcs amend -a foo -A new_author --keep-date | grep -i 'amending changes'
darcs changes --last=1 | head -n 3 | grep Date > new_date
cmp old_date new_date

cd ..

# check that the identity changes with --keep-date
darcs get temp1 temp2
cd temp2

echo "another line" >> foo
darcs changes --last=1 | head -n 1 > old_date
echo y | darcs amend -a foo -A new_author --keep-date | grep -i 'amending changes'
darcs pull ../temp1 -a --skip-conflicts | grep -i "Skipping some"

cd ..

rm -rf temp1 temp2


# This checks for a possible bug in patch selection where the no available
# patches case is hit.

darcs init temp1
cd temp1
touch A
darcs record -lam A
echo 'l1' >> A
darcs record -am l1
darcs amend -a --patch 'A'

cd ..
rm -rf temp1

## Copyright (C) 2011 Ganesh Sittampalam <ganesh@earth.li>

darcs init temp1
cd temp1

echo 'file1' > file1
darcs record -lam 'file1'

echo 'file2' > file2
darcs record -lam 'file2'

echo 'file2:amended' > file2
echo 'nkya' | darcs amend

darcs log -p 'file2' -v | grep amended
cd ..
rm -rf temp1

## Test for amend --unrecord
## Copyright (C) 2012  Ganesh Sittampalam

darcs init temp1
cd temp1

echo -e "x\ny" > foo
darcs rec -lam "add foo"

echo -e "1\nx\ny\n2" > foo
darcs rec -am "insert 1 and 2"

echo yyny | darcs amend --unrecord
echo -e "x\ny\n2" > foo.expected
darcs show contents foo | diff -q foo.expected -

echo yenyy | DARCS_EDITOR="sed -i -e s/2/2j/" darcs amend --unrecord
echo -e "x\ny\n2j" > foo.expected
darcs show contents foo | diff -q foo.expected -

echo 'ugh' > bar
darcs add bar
# use amend to check it's still a short form for amend-record
# if we make amend-unrecord visible rather than hidden that would change
echo y | darcs amend -a
darcs show contents bar | diff -q bar -

# test that amend --unrecord --all and specifying files works
echo y | darcs amend --unrecord -a foo
echo -e "x\ny" > foo.expected
darcs show contents foo | diff -q foo.expected -
darcs show contents bar | diff -q bar -

cd ..
rm -rf temp1
