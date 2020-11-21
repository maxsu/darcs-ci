#!/usr/bin/env bash
. ./lib

darcs init temp1
cd temp1

echo hi world > temp.c
darcs add temp.c
darcs record --all -A test --name=hi
echo goodbye >> temp.c
darcs whatsnew
darcs record -a -A au -m bye
echo bar > bar.c
darcs add bar.c
darcs record -a -m one -A ex
darcs mv bar.c zig.c
darcs whatsnew
darcs record -a -m two -A ex
mkdir baz
darcs add baz
darcs whatsnew
darcs record -a -m three -A ex
darcs mv zig.c baz/bar.c
darcs whatsnew
darcs record -a -m four -A ex
darcs mv baz temp
darcs whatsnew
darcs record -a -m five -A ex

darcs mv temp temp 1> stdout 2> stderr || true
grep 'Cannot rename a file or directory onto itself' stderr

cd ..
rm -rf temp1

darcs init temp1
cd temp1

echo hi world > a
darcs record -lam lower
cd ..
darcs clone temp1 temp2
cd temp1
darcs mv a A
echo goodbye > A
darcs record --all -m 'to upper'
cd ../temp2
darcs pull -a

cd ..
rm -rf temp1 temp2

# Part 2

darcs init temp1
cd temp1

echo adding a directory with more than one .. in it should work.
mkdir foo.d
mkdir foo.d/second
mkdir foo.d/second/third
mkdir foo.d/other

touch ./foo.d/other/date.t
darcs add -r foo.d

cd foo.d/second/third

darcs mv ../../other/date.t ../../other/date_moved.t

cd ../../..
echo darcs refuses to move to an existing file
touch ping pong
darcs add ping pong

not darcs mv ping pong 2>&1 | grep "already exists"

# case sensitivity series
# -----------------------
# these are tests designed to check out darcs behave wrt to renames
# where the case of the file becomes important

# are we on a case sensitive file system?
touch is_it_cs
rm -f IS_IT_CS

if test -e is_it_cs; then
  echo This is a case-sensitive file system.
else
  echo This is NOT a case-sensitive file system.
fi

# if the new file already exists - we don't allow it
# basically the same test as mv ping pong, except we do mv ping PING
# and both ping and PING exist on the filesystem
echo "case sensitivity - simply don't allow mv if new file exists"
touch 'cs-n-1'; touch 'CS-N-1';
touch 'cs-y-1'; touch 'CS-Y-1';
darcs add cs-n-1 cs-y-1

if test -e is_it_cs; then
  # regardless of case-ok, we do NOT want this mv at all
  not darcs mv cs-n-1 CS-Y-1 2>&1 | grep "already exists"

  not darcs mv --case-ok cs-n-1 CS-Y-1 2>&1 | grep "already exists"
fi

# if the new file does not already exist - we allow it
echo "case sensitivity - the new file does *not* exist"
touch 'cs-n-2';
touch 'cs-y-2';
darcs add cs-n-2 cs-y-2
# these mv's should be allowed regardless of flag or filesystem
darcs mv cs-n-2 CS-N-2
darcs mv --case-ok cs-y-2 CS-Y-2

# parasites - do not accidentally overwrite a file just because it has a
# similar name and points to the same inode.  We want to check if a file if the
# same NAME already exists - we shouldn't care about what the actual file is!
echo "case sensitivity - inode check";
touch 'cs-n-3';
touch 'cs-y-3';
darcs add cs-n-3 cs-y-3

if ln cs-n-3 CS-N-3; then # checking if we support hard links
  ln cs-y-3 CS-Y-3
  # regardless of case-ok, we do NOT want this mv at all
  not darcs mv cs-n-3 CS-N-3 2>&1 | grep "already exists"

  not darcs mv --case-ok cs-y-3 CS-Y-3 2>&1 | grep "already exists"
fi

# parasites - we don't allow weird stuff like mv foo bar/foo just because
# we opened up some crazy exception based on foo's name
echo 'refuses to move to an existing file with same name, different path'
touch 'cs-n-4'; touch 'foo.d/cs-n-4';
touch 'cs-y-4'; touch 'foo.d/cs-y-4';
darcs add cs-n-4
# regardless of case-ok, we do NOT want this mv at all
not darcs mv cs-n-4 foo.d/cs-n-4 2>&1 | grep "already exists"

not darcs mv --case-ok cs-y-4 foo.d/cs-y-4 2>&1 | grep "unadded"

# ---------------------------
# end case sensitivity series

touch abs_path.t
darcs add abs_path.t
REPO_ABS=`pwd`
darcs mv "$REPO_ABS/abs_path.t" abs_path_new.t
darcs mv abs_path_new.t "$REPO_ABS/abs_path.t"

# issue608

touch 'gonna_be_deleted';
darcs add gonna_be_deleted
darcs record -am 'added doomed file'
rm gonna_be_deleted
darcs record -am 'deleted file'
touch 'new_file';
darcs add new_file
darcs mv new_file gonna_be_deleted

cd ..
rm -rf temp1

# mv and test suite

darcs init temp1
cd temp1

date > foo
darcs record -lam add_foo

echo 'test ! -e foo' > test.sh # "foo should not exist"
darcs record -lam add_test

darcs setpref test 'ls && bash test.sh'
darcs record -a -m settest --no-test

darcs mv foo bar
darcs record --debug -a -m mvfoo

cd ..
rm -rf temp1

# mv then ad

darcs init temp1
cd temp1

touch fee fi fo fum
darcs record -lam add
darcs mv fee foo
touch fee
darcs add fee
darcs record -a -m newfee
darcs mv fi fib
darcs record -a -m mvfi
date > fi
darcs add fi 
darcs record -a -m newfi

cd ..
rm -rf temp1

# illegal mv

darcs init temp1
cd temp1

echo text > afile.txt
darcs record -lam init
mkdir d
not darcs mv afile.txt d/afile.txt # should fail, since d not in repo

cd ..
rm -rf temp1

# swapping files

darcs init temp1
cd temp1

touch foo bar
darcs record -lam add_foo_bar
darcs mv foo zig
darcs mv bar foo
darcs mv zig bar
darcs record -a -m swap_foo_bar
cd ..
rm -rf temp1


## issue2139 - darcs should accept to mv to the current working directory
## Copyright (C) 2012 Eric Kow

darcs init temp1
cd temp1

# move dir to root
mkdir a a/a2 a/a3
darcs record -lam 'Some directories (a)'
darcs mv a/a2 .
test -d a2 
cd a
darcs mv a3 ..
not test -d a3
cd ..
test -d a3

# move dir to non-root dir
mkdir b b2 b3
darcs record -lam 'Some directories (b)'
darcs mv b2 b
test -d b/b2
cd b
darcs mv ../b3 .
test -d b3
cd ..

cd ..
rm -rf temp1

# issue1740 - darcs mv on directories should work after the fact

darcs init temp1
cd temp1

mkdir d
echo 'Example content.' > d/f
darcs record -lam 'Add d/f'
mv d d2
darcs mv d d2 # oops, I meant to darcs mv that
darcs what | grep "move ./d ./d2"
cd ..
rm -rf temp1

# it should not crash when given invalid paths
# instead issue a proper error message

rm -rf R
darcs init R
cd R
touch f
darcs record -lam 'add f' f
# 2 path arguments
# target does not exist: OK
darcs move f g
darcs revert -a
# source does not exist: Fail
not darcs move g f >LOG 2>&1
not grep -i bug LOG
grep -i 'does not exist' LOG
# 2nd is un-added existing directory
mkdir d
not darcs move f d >LOG 2>&1
not grep -i bug LOG
grep -i 'not known' LOG
# 3 path arguments
touch g
darcs record -lam 'add g' g
# 3rd arg is un-added existing dir
not darcs move f g d >LOG 2>&1
not grep -i bug LOG
grep -i "isn't known" LOG
# 3rd argument does not exist
rm -rf d
not darcs move f g d >LOG 2>&1
not grep -i bug LOG
grep -i "isn't known" LOG
cd ..
