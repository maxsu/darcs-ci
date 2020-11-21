#!/usr/bin/env bash

. lib

rm -rf R S T

darcs init R
cd R
touch f
darcs record -l f -a -m 'baseline'
darcs clone . ../S
darcs clone . ../T
echo R > f
darcs record -l f -a -m 'hunk R'
cd ../S
echo S > f
darcs record -l f -a -m 'hunk S'
cd ../T
echo T > f
darcs record -l f -a -m 'hunk T'
cd ../R
darcs pull -a --allow-conflicts ../S ../T
# echo X > f
# darcs record -l f -a -m 'resolve conflicts'
cd ../S
darcs pull -a ../R --allow-conflicts
cd ../R
darcs log -v > ../before_rebase
darcs rebase suspend -a
cp _darcs/rebase ..
darcs rebase unsuspend -a
darcs log -v > ../after_rebase
cd ..
