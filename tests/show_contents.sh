#!/usr/bin/env bash
. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo
echo first > foo
darcs record -a -m "first edit" -A author1
echo second > foo
darcs record -a -m "second edit" -A author2
darcs tag t1 -A moi
echo third > foo
darcs record -a -m "third edit" -A author3
echo fourth > foo
darcs record -a -m "fourth edit" -A author4
echo unrecorded > foo
darcs show contents foo | grep fourth
darcs show contents foo -p third | grep third
darcs show contents foo --match="author author1" first | grep first
darcs show contents foo --tag t1 | grep second
not darcs show contents foo --match "hash bla" 2>&1 | tee out
grep '"hash bla"' out
darcs show contents -n 2 foo | grep third
cd ..

rm -rf temp1

## issue1705 - darcs show contents --index=1 => darcs failed:  Pattern not specified in get_nonrange_match

darcs init temp1
cd temp1
echo 111 > 1
darcs record -lam 'add file 1'
darcs show contents --index=1 1
cd ..
rm -rf temp1

## issue2447 - get contents of deleted file 

darcs init temp1
cd temp1

echo 'example content' > f
darcs record -lam 'add f'
hash1=$(darcs log --last=1 | grep '^patch' | cut -d ' ' -f 2)
rm f
darcs record -am 'removed f'
darcs show contents --hash $hash1 f | grep 'example content'

mkdir d
echo 'example content' > d/f
darcs record -lam 'add d/f'
hash2=$(darcs log --last=1 | grep '^patch' | cut -d ' ' -f 2)
rm d/f
darcs record -am 'removed d/f'
darcs show contents --hash $hash2 d/f | grep 'example content'

darcs obliterate -a --last=1

rm -rf d
darcs record -am 'removed d and d/f'
darcs show contents --hash $hash2 d/f | grep 'example content'
cd ..
rm -rf temp1
