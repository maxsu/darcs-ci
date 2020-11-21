#!/usr/bin/env bash

## Test that we can upgrade an old-style rebase repo
## where the suspended patch had a conflict

. lib

rm -rf R log
unpack_testdata old-style-rebase-conflict

cd R
darcs rebase upgrade
darcs rebase log 2>../log
grep 'Rebase in progress: 1 suspended patch' ../log
cd ..
