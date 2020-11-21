#!/usr/bin/env bash

. lib

# add a file, turn it into a directory in the working tree,
# then let darcs figure out how to handle that

rm -rf R
darcs init R
cd R
touch f
darcs add f
rm f
mkdir f
darcs whatsnew -l --no-summary | tee ../before
darcs record -lam 'patchname'
darcs log -v | tee ../after
for log in ../before ../after; do
  grep 'adddir ./f' $log
  not grep 'addfile ./f$' $log
done
cd ..

# same with dir and file swapped

rm -rf R
darcs init R
cd R
mkdir f
# for good measure add a file to the directory
touch f/g
darcs add -r f
rm -rf f
touch f
darcs whatsnew -l --no-summary | tee ../before
darcs record -vlam 'patchname'
darcs log -v | tee ../after
for log in ../before ../after; do
  grep 'addfile ./f' $log
  not grep 'adddir ./f' $log
done
cd ..
