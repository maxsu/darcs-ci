#!/usr/bin/env bash

# Some tests for 'darcs record '

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init

# issue308 - no patches and no deps for record should abort
darcs record -am foo --ask-deps | grep -i "Ok, if you don't want to record anything, that's fine!"

# RT#476 - --ask-deps works when there are no patches
if echo $OS | grep -i windows; then
  echo This test does not work on Windows
else
  touch t.f
  darcs add t.f
  darcs record  -am add
  echo a | darcs record  -am foo --ask-deps | grep -i 'finished recording'
fi

# RT#231 - special message is given for nonexistent directories
not darcs record -am foo not_there.txt > log 2>&1
grep -i 'non-existing' log

# RT#231 - a nonexistent file before an existing file is handled correctly
# test disabled, see tests/issue2494-output-of-record-with-file-arguments.sh
# which contains an updated test
# touch b.t
# darcs record  -lam foo a.t b.t > log
# grep -i 'WARNING:.*a.t' log
# grep -iv 'WARNING:.*b.t' log

DIR="`pwd`"
touch date.t
darcs add date.t
darcs record -a -m foo "$DIR/date.t" | grep -i 'finished recording'

# issue396 - record -l ""
touch 'notnull.t'
darcs record  -am foo -l "" notnull.t | grep -i 'finished recording'

# basic record
date >> date.t
darcs record -a -m basic_record date.t | grep -i 'finished recording'

# testing --logfile
date >> date.t
echo "second record\n" >> log.txt
darcs record  -a -m 'second record' --logfile=log.txt  date.t | grep -i 'finished recording'

echo text > file
darcs add file

# refuse empty patch name

# true command outputs nothing & ignores its arguments
DARCS_EDITOR=true not darcs record -a --edit-long-comment
# cat command reproduces its input unchanged
DARCS_EDITOR=cat not darcs record -a -m "" --edit-long-comment
not darcs record -a -m ""

# refuse patch name starting with "TAG "

# editor output will be "echo TAG _darcs/patch_description.txt"
DARCS_EDITOR='echo TAG ' not darcs record -a --edit-long-comment
# cat command reproduces its input unchanged
DARCS_EDITOR=cat not darcs record -a -m "TAG fake" --edit-long-comment
not darcs record -a -m "TAG fake"

cd ..


# record race

rm -rf foo1 foo2
mkdir foo1 foo2
cd foo1
darcs init
echo zig > foo
darcs add foo
sleep 1
darcs record -a -m add_foo -A x
#sleep 1
echo zag >> foo
darcs record --ignore-time -a -m mod_foo -A x
cd ../foo2
darcs init
darcs pull -a ../foo1
cd ..
cmp foo1/foo foo2/foo

# record interactive


rm -rf temp1
mkdir temp1
cd temp1
darcs init

touch foo
darcs add foo
darcs record -a -m addfoo

darcs replace one two foo
darcs replace three four foo
darcs replace five six foo

echo sa | darcs record -m cancelled

darcs whatsnew

darcs changes > ch
not grep cancelled ch

cd ..


# Some tests for 'darcs rec --edit-long-comment'

rm -rf temp1

export DARCS_EDITOR="cat -n"
# editor: space in command
rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch file.t
darcs add file.t
echo y | darcs record --edit-long-comment -a -m foo file.t | grep '# Please enter'
cd ..

# editor: space in path
rm -rf temp2\ dir
mkdir temp2\ dir
cd temp2\ dir
darcs init
touch file.t
darcs add file.t
echo y | darcs record --edit-long-comment -a -m foo file.t | grep '# Please enter'
cd ..

# make sure summaries are coalesced
rm -rf temp3
mkdir temp3
cd temp3
darcs init
cat > file <<EOF
1
2
3
4
EOF
darcs add file
darcs rec -a -m "init" file
cat > file <<EOF
A
2
3
B
EOF
echo y | darcs record --edit-long-comment -a -m edit | grep -c "./file" | grep 1
cd ..

export DARCS_EDITOR='grep "# Please enter"'
# editor: quoting in command
rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch file.t
darcs add file.t
echo y | darcs record --edit-long-comment -a -m foo file.t | grep '# Please enter'
cd ..

export DARCS_EDITOR='echo'
# editor: evil filename
rm -rf temp1
darcs init temp1
cd temp1
touch file.t
darcs add file.t
touch '; test-command'
echo > test-command << FOO
#!/bin/sh
echo EVIL
FOO
chmod u+x test-command
echo y | darcs record --logfile='; test-command' --edit-long-comment -a -m foo file.t > log
not grep EVIL log
cd ..

## Test for issue142 - darcs record --logfile foo should not

rm -rf temp1
darcs init temp1
cd temp1
touch f g
touch log
darcs     record -alm f --logfile log     f
not darcs record -alm g --logfile missing g
cd ..

## Test for issue1845 - darcs record f, for f a removed file should work
## Public domain - 2010 Petr Rockai

rm -rf temp1
darcs init temp1
cd temp1

echo 'Example content.' > f
darcs rec -lam "first"
rm -f f
echo ny | darcs record f 2>&1 | tee log
not grep "None of the files" log
cd ..

# issue1472 - "darcs record ./foo" shouldn't even TRY to read ./bar

rm -rf temp1
darcs init temp1
mkdir temp1/d/
echo 'Example content.' >temp1/f
echo 'Similar content.' >temp1/d/f
chmod 0 temp1/f # Make temp1/f unreadable
darcs record    --repo temp1 -lam 'Only changes to temp1/d/.' d

# issue1871 - `darcs record .` should work for tracked changes
# in a subdirectory even if the subdirectory itself is not known yet.
rm -rf temp1
darcs init temp1
cd temp1
mkdir d
echo 'Example content.' > d/f
darcs add d/f
echo ny | darcs record
echo ny | darcs record . > log
not grep "None of the files" log
cd ..
