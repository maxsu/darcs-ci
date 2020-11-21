#!/usr/bin/env bash

# Some tests for 'darcs push'

. lib

slash() {
if echo $OS | grep -q -i windows; then
    echo -n \\
else
    echo -n /
fi
}

DIR="`pwd`"

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
cd ..
mkdir temp2
cd temp2
darcs init
cd ..

# push without a repo gives an error
cd temp1
not darcs push -p 123 2> log
grep -i 'No default repository to push to' log
cd ..

mkdir -p temp2/one/two
cd temp2/one/two
# darcs push should work relative to the current directory
darcs push -a ../../../temp1 | grep -i 'No recorded local patches to push'
cd ../../../

# darcs push should push into repo specified with --repo
cd temp2
darcs add one
darcs record --name uno --all
cd ..

darcs push --repodir temp2 --all temp1 | grep -i 'Finished apply'

cd temp1
# Before trying to pull from self, defaultrepo does not exist
test ! -e _darcs/prefs/defaultrepo
# return special message when you try to push to yourself
not darcs push -a "$DIR`slash`temp1" 2> log
grep -i "cannot push from repository to itself" log
# and don't update the default repo to be the current dir
test ! -e _darcs/prefs/defaultrepo
cd ..

rm -rf temp1 temp2

# Check that the right patches get pushed using --dont-prompt-for-dependencies

rm -rf temp1 temp2
darcs init temp2
darcs init temp1

cd temp1
echo foo > f
darcs record -alm foo1
echo bar > b
darcs rec -alm bar1
echo foo2 > f
darcs record -alm foo2
echo bar2 > b
darcs record -alm bar2
echo yy | darcs push ../temp2 --dont-prompt-for-dependencies -p foo2 --dry-run -i > toto
#on the previous line, we get asked about foo2, and we take it
grep foo2 toto | wc -l | grep 2
#we don't get asked about foo1, but we take it anyway, so 
grep foo1 toto | wc -l | grep 1
#and we don't send bar
not grep bar toto
cd ..
rm -rf temp1 temp2

# For issue257: push => incorrect return code when couldn't get lock

rm -rf temp1 temp2
darcs init temp1
cd temp1
echo foo > foo.c
darcs rec -alm init
cd ..
darcs clone temp1 temp2
cd temp2
echo server >> foo.c
darcs rec -alm server
cd ../temp1
echo client >> foo.c
darcs rec -alm client
not darcs push -a ../temp2
cd ..
rm -rf temp1 temp2
