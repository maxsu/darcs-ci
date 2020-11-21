#!/usr/bin/env bash

. lib

rm -rf temp1 temp2

darcs init temp1
cd temp1

cd ..
darcs init temp2
cd temp2

mkdir one
cd one
mkdir two
cd two
# darcs pull should work relative to the current directory
darcs pull -a ../../../temp1 | grep -i 'No remote patches to pull in'

# darcs pull should pull into repo specified with --repo
cd ../..  # now in temp2
darcs add one;
darcs record --name uno --all
cd ..     # now outside of any repo
darcs pull --set-default --repodir temp1 --all temp2 | grep -i 'Finished pulling.' # temp2 is not relative to temp1

# set up server repo
date > temp2/one/date.t
darcs add --repodir ./temp2 one/date.t
darcs record --repodir ./temp2 -a -m foo

# set up client repo for failure
if echo $OS | grep -i windows; then
    echo this test does not work on windows because it
    echo is not possible to chmod -r
elif whoami | grep root; then
    echo root never gets permission denied
else
    chmod a-rwx ./temp1/one # remove all permissions
    not darcs pull --repodir ./temp1 -a 2> err
    chmod u+rwx temp1/one # restore permission
    grep 'permission denied' err
    rm -rf temp1/one
fi

cd temp1

echo Before trying to pull from self, defaultrepo is something else
not grep temp1 _darcs/prefs/defaultrepo

#return special message when you try to pull from yourself
DIR="`pwd`"
not darcs pull --debug -a "$DIR" 2> out
grep 'Can.t pull from current repository' out

not darcs pull --debug -a . 2> out
grep 'Can.t pull from current repository' out

# and do not update the default repo to be the current di
not grep temp1 _darcs/prefs/defaultrepo

rm -f _darcs/prefs/defaultrepo
not darcs pull 2> err
grep 'please specify one' err
echo . > _darcs/prefs/defaultrepo
not darcs pull --debug 2> err
grep 'Can.t pull from current repository' err

not darcs pull --debug ../* 2> out
not grep 'Can.t pull from current repository' out
cd .. # now outside of any repo

cd temp1
echo a > foo
darcs record -lam AA
echo b > foo
darcs record -lam BB
echo c > foo
darcs record -lam CC
darcs rollback -p CC -a
darcs record -am unC
cd ..
rm -rf temp2
darcs get --to-patch B temp1 temp2
cd temp2
sleep 1 # So that rollback won't have same timestamp as get.
darcs rollback -p BB -a
darcs record -am unB
darcs pull -a ../temp1 2> err2
not grep 'Error applying patch' err2
cd ..

cd temp1
echo -n foo > baz
darcs add baz
darcs record  -am newbaz
cd ../temp2
darcs pull -a | grep Finished
echo -n bar > baz
darcs record  -am bazbar
cd ../temp1
darcs pull ../temp2 -a
echo -n bar > correct_baz
diff baz correct_baz
cd ..

#   my $test_name = "when a patch creating a directory is attempted to be applied
#       while a directory with that name already exists, a warning is raised, but
#       the pull succeeds.";
mkdir temp1/newdir
cd temp1
darcs add newdir
darcs record -am newdir
cd ../temp2
mkdir newdir
darcs pull -a --set-default ../temp1 &> out2
grep Backing out2
grep 'Finished pulling' out2
grep newdir out2
cd ..

rm -rf temp1 temp2


# issue662, which triggered:
#  darcs failed:  Error applying hunk to file ./t.t
#  Error applying patch to the working tree.

darcs init temp1
cd temp1

touch t.t
echo 'content'> t.t
darcs record -lam 'initial add'
echo 'content: remote change'> t.t
darcs record -am 'remote change' --ignore
cd ..
darcs clone temp1 temp2
cd temp2
darcs obliterate --last 1 --all
echo 'content: local change'> t.t
# this is now recognized as a conflict with working:
echo y | darcs pull -a ../temp1
darcs w -s
darcs revert -a
cd ..
rm -rf temp1 temp2

# pull with conflicts

darcs initialize temp1
cd temp1
echo foo > bar
darcs record -lam addbar

cd ..
darcs clone temp1 temp2
cd temp1
date > bar
darcs record -a -m datebar
cd ../temp2
echo aack >> bar
darcs record -a -m aackbar
darcs pull -a
darcs check

cd ..
rm -rf temp1 temp2

# pull --union

rm -rf temp1 temp2 temp3

darcs init temp1
cd temp1
echo A > A
darcs record -lam A
echo B > B
darcs record -lam B

cd ..
darcs clone temp1 temp2

cd temp2
darcs obliterate --last 1 -a
echo C > C
darcs record -lam C
cd ..

darcs init temp3
cd temp3
darcs pull -a ../temp1 ../temp2

darcs log > out
grep A out
grep B out
grep C out

cd ..
rm -rf temp1 temp2 temp3

# pull --intersection

darcs init temp1
cd temp1
echo A > A
darcs record -lam Aismyname
echo B > B
darcs record -lam Bismyname

cd ..
darcs clone temp1 temp2
cd temp2
darcs obliterate --last 1 -a
echo C > C
darcs record -lam Cismyname

cd ..
darcs init temp3
cd temp3
darcs pull -a --intersection ../temp1 ../temp2
darcs log > out
grep Aismyname out
not grep Bismyname out
not grep Cismyname out

cd ..
rm -rf temp1 temp2 temp3


# pull --skip-conflicts
rm -rf R S
darcs init R
cd R
echo 'foo' > foo
echo 'bar' > bar
darcs rec -lam 'Add foo and bar'
cd ..

darcs clone R S

cd R
echo 'foo2' > foo
darcs rec -lam 'Change foo (2)'
echo 'bar2' > bar
darcs rec -lam 'Change bar (2)'
cd ..

cd S
echo 'foo3' > foo
darcs rec -lam 'Change foo (3)'
darcs pull --skip-conflicts -a ../R
test `darcs log --count` -eq 3
cd ..

cd S
darcs pull -a ../R
test `darcs log --count` -eq 4
cd ..
rm -rf R S

# bad pending after pull

rm -fr temp1 temp2

darcs init temp1
cd temp1
echo abc > A
echo def > B1
darcs record -lam patch1
darcs mv B1 B2
darcs record -am patch2
cd ..

darcs init temp2
cd temp2
darcs pull -a ../temp1
not darcs whatsnew
cd ..

rm -fr temp1 temp2

# issue494: note that in this test, we deliberately select filenames
# with a backwards sorting order
darcs init temp1
cd temp1
echo abc > b
darcs record -lam patch1
darcs mv b a
echo def > a
darcs record -am patch2
cd ..

darcs init temp2
cd temp2
darcs pull --all ../temp1
not darcs whatsnew
cd ..

rm -fr temp1 temp2

# pull binary

rm -rf temp1 temp2

darcs init temp1
cd temp1
printf "%01048576d" 0 > foo
darcs record -l -a -m xx
rm foo
darcs record -a -m yy
cd ..

darcs init temp2
cd temp2
echo yny | darcs pull --set-default ../temp1
rm foo
# for darcs-1 and darcs-3 this pull conflicts with unrecorded changes
# (which is correct, whereas darcs-2 is buggy)
if test "$format" != "darcs-2"; then
  echo y | darcs pull -a
else
  darcs pull -a
fi
cd ..

rm -rf temp1 temp2

# pull --matches
darcs init temp1
cd temp1
echo first > a
darcs record -lam 'first'
firsthash=`darcs log --xml | grep 'hash=' | sed -e "s/.*hash='//" -e "s/'>//"`
echo second > b
darcs record -lam 'second'
cd ..

darcs init temp2
darcs pull --repodir temp2 -a --match "hash $firsthash" temp1
test $(darcs log --count --repodir temp2) -eq 1

darcs init temp3
darcs pull --repodir temp3 -a --hash $firsthash temp1
test $(darcs log --count --repodir temp3) -eq 1

rm -rf temp1 temp2 temp3
