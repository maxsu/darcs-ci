#!/usr/bin/env bash

# For issue68, 'don't report "resource vanished" when stdout pipe is broken.'

. ./lib

rm -rf temp1 # Another script may have left a mess.

darcs init --repodir temp1

cd temp1

for i in {1..500}
do
    echo $i >> f
done

darcs changes 2> err | head
darcs rec -alm 'Add big f'

# We recorded a big file add, so asking for the first n lines of the patch
# would trigger this bug.
darcs changes -v 2> err | head

[[ ! -s err ]]
