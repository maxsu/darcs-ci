. lib
rm -rf R
darcs init R
cd R
touch foo bar
darcs rec -lam'add foo and bar'
darcs show files --no-files
