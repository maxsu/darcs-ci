#!/usr/bin/env bash

. lib

# test that 'whatsnew -l --boring' actually lists boring files

rm -rf R
darcs init R
cd R

echo xxx > boring
darcs setpref boringfile boring
darcs record -lam'added boring and set as boringile'
touch xxx
darcs whatsnew -l --boring | grep xxx
darcs whatsnew -l --boring | grep -v 'No changes'

cd ..
rm -rf R
