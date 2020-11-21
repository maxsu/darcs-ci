#!/usr/bin/env bash
. ./lib

rm -rf temp1 temp2
mkdir temp1 temp2

cd temp2
darcs init
cd ..

cd temp1
darcs init
date > foobar
darcs add foobar
darcs rec -a -m add-foobar

cp $TESTBIN/sendmail.hs .

darcs send --mail \
    --author=me -a --to=random@random \
    --sendmail-command="runghc sendmail.hs %s %t %c %b %f %a %S %T %C %B %F %A something" ../temp2

cat saved.out
grep add-foobar saved.out
grep 'addfile ./foobar' saved.out

cd ..
