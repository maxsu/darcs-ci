#!/usr/bin/env bash

. lib

mkdir future
cd future
darcs  init
touch titi
darcs add titi
darcs record -am titi
sed -i 's/hashed/hashed\|gobbledygook/' _darcs/format
cat _darcs/format
cd ..

# get future repo: should be ok
darcs get future temp1
cd temp1
darcs changes
touch toto
darcs add toto
darcs record -am 'blah'
cd ..
