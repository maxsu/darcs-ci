## Test for issue1956

# copied almost verbatim from igloo's
# http://bugs.darcs.net/file1862/darcs_bug_with_touch.sh

. lib

rm -rf a
rm -rf b

mkdir a
cd a
darcs init --darcs-2
echo A > file
darcs add file
darcs rec -a -m A --ignore-times
cd ..

darcs get a b

cd a
echo BB > file
darcs rec -a -m B --ignore-times
cd ..

sleep 1

cd b
touch ts
echo R > file
touch file --reference=ts
darcs rec -a -m R
# sleep 1
echo S > file
touch file --reference=ts
# darcs rec -a -m S --ignore-times
echo y| darcs pull ../a -ap B
cd ..
