#!/usr/bin/env bash
. ./lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo utrecht > aargh
darcs add aargh

not darcs wh foo foo/../foo/.

cd ..
rm -rf temp1

