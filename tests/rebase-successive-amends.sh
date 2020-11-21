#!/usr/bin/env bash

. lib

rm -rf R
darcs init R
cd R
echo A > file
darcs add file
darcs record file -am A
echo B > file
darcs record file -am B
echo C > file
darcs record file -am C
darcs rebase suspend -a -p C
echo B1 > file
echo yd | darcs amend -am B1
echo B2 > file
echo yd | darcs amend -am B2
echo B3 > file
echo yd | darcs amend -am B3
cp _darcs/rebase ../1-before-unsuspend
darcs rebase unsuspend -a 2>../log
grep conflicts ../log
cd ..
