#!/usr/bin/env bash

# Test for issue2634: suspending and unsuspending conflicted patches

. ./lib

# prepare: make two conflicting patches one and two and merge them
rm -rf R S T1 T2
darcs init R
cd R
touch file
darcs record -l file -am 'base'
echo one > file
darcs record file -am 'one'
darcs clone . ../S
echo two > file
echo y|darcs amend -am 'two' -p one
echo y|darcs pull ../S -a --allow-conflicts --reorder-patches
# patch order should be init; one; two
cd ..

# test1: suspend both 'one' and 'two', rebase obliterate 'one', then unsuspend
rm -rf T1
darcs clone R T1
cd T1
# get rid of conflict markup
darcs revert -a
echo yydyy | darcs rebase suspend
echo yd|darcs rebase obliterate
darcs rebase unsuspend -p two -a
cat file >&2
not grep one file
grep two file
cd ..

# test2: suspend 'two' and obliterate 'one', then unsuspend
rm -rf T2
darcs clone R T2
cd T2
# get rid of conflict markup
darcs revert -a
echo ydy | darcs rebase suspend
echo y|darcs obliterate -p one -a
darcs rebase unsuspend -p two -a
cat file >&2
not grep one file
grep two file
cd ..

# prepare: three-way conflict
rm -rf U
darcs clone S U
cd U
echo three > file
echo y|darcs amend -am 'three' -p one
echo y|darcs pull ../R -a --allow-conflicts --reorder-patches
# patch order should be init; one; two; three
cd ..

# test3: suspend 'three', obliterate 'two', then unsuspend
rm -rf T3
darcs clone U T3
cd T3
# get rid of conflict markup
darcs revert -a
echo ydy | darcs rebase suspend
echo y|darcs obliterate -p two -a
darcs rebase unsuspend -p three -a
cat file >&2
grep one file
not grep two file
grep three file
cd ..

# test4: suspend all, rebase obliterate 'one', then unsuspend
rm -rf T4
darcs clone U T4
cd T4
# get rid of conflict markup
darcs revert -a
echo yyydyyy | darcs rebase suspend
echo yd|darcs rebase obliterate
darcs rebase unsuspend -a
cat file >&2
not grep one file
grep two file
grep three file
cd ..

# test5: suspend all, rebase obliterate 'one' and 'two', then unsuspend
rm -rf T5
darcs clone U T5
cd T5
# get rid of conflict markup
darcs revert -a
echo yyydyyy | darcs rebase suspend
echo yyd|darcs rebase obliterate
darcs rebase unsuspend -a
cat file >&2
not grep one file
not grep two file
grep three file
cd ..
