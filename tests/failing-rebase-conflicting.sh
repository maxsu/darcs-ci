#!/usr/bin/env bash

. lib

rm -rf R S

darcs init R
cd R
echo R > f
darcs record -l f -a -m 'add f in R'
cd ..
darcs init S
cd S
echo S > f
darcs record -l f -a -m 'add f in S'
cd ../R
darcs pull -a --allow-conflicts ../S
echo X > f
darcs record -l f -a -m 'resolve conflict'
darcs push -a ../S
darcs log -v > ../before_rebase
darcs rebase suspend -a
darcs rebase unsuspend -a
darcs log -v > ../after_rebase
cd ..
diff R/f S/f
