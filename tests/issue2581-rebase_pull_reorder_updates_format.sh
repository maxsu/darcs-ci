. lib

darcs init R
cd R
touch f
darcs record -l f -am "add f in R"
cd ..

darcs init S
cd S
touch f
darcs record -l f -am "add f in S"

# first y is for the "repos are unrelated" prompt
echo yaa | darcs rebase pull ../R --reorder-patches
darcs rebase log
