#!/usr/bin/env bash
. ./lib

# for issue706: "Filenames with spaces issue"

rm -rf temp
mkdir temp
cd temp
darcs init

touch 'A B'

darcs add 'A B'
darcs rec -a -m 'a b' -A me
ls
darcs check

cd ..
rm -rf temp
