#!/usr/bin/env bash

## Test that darcs can apply a patch bundle where the tag in the
## context of the bundle is unclean in the current repository.

. lib

rm -rf R S T

darcs init R
cd R
# so that we have something to tag, a tag of an empty repo
# seems to be ignored
echo 'initial content' > initial
darcs rec -lam "initial content"
darcs tag initial

echo A > A
darcs rec -lam "change A"
echo B > B
darcs rec -lam "change B"

cd ..

darcs init S
cd S
# pull the tag + A, but not B
darcs pull ../R -a --tag=initial
darcs pull ../R -a -p 'change A'

cd ../R
# create b.dpatch
darcs send -a -O --no-edit-description ../S

cd ..
mkdir T
cd T
darcs init
# pull the first patch + A, but not the tag or B
darcs pull ../R -a -p 'initial content'
darcs pull ../R -a -p 'change A'
# pull the tag, so now it's unclean
darcs pull ../R -a --tag 'initial'

darcs apply ../R/change-b.dpatch
