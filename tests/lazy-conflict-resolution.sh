# this tests that when we resolve conflicts, we don't access
# patches in the context if they are not needed.

. lib

rm -rf R S
darcs init R
cd R
touch file
darcs record -l file -a -m add
darcs tag mytag
rm _darcs/patches/*
echo xxx > file
darcs record -l file -a -m edit1
darcs clone --lazy . ../S
echo yyy > file
echo y | darcs amend -a -m edit2
darcs pull -a ../S
cd ..
