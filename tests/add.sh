#!/usr/bin/env bash
. ./lib

rm -rf temp1
darcs init temp1
cd temp1
touch foo bar
darcs add foo bar

for (( i=0 ; i < 5; i=i+1 )); do
  echo $i >> file-$i;
  darcs add file-$i
done

cd ..

rm -rf temp1

# add in subdir

darcs init temp1
cd temp1

mkdir dir
echo zig > dir/foo
darcs add dir dir/foo
darcs record -am add_foo
cd ..
rm -rf temp1

# addrm

darcs init temp1
cd temp1
touch foo
darcs add foo
darcs record -a -m add_foo -A x
darcs remove foo
darcs record -a -m del_foo -A x
cd ..
rm -rf temp1

# issue1162: add nonexistent slash

rm -rf temp1
darcs init temp1
cd temp1
not darcs add a/ 2> err
cat err
grep 'does not exist' err
cd ..
rm -rf temp1

# issue184: recording files in directories that haven't explicity been added

darcs init temp1
cd temp1

mkdir new
mkdir new/dir
touch new/dir/t.t
darcs add new/dir/t.t
darcs record -am test new/dir/t.t > log
not grep "don't want to record" log
cd ..

rm -rf temp1

# Make sure that parent directories are added for files
darcs init temp1
cd temp1

mkdir -p a.d/aa.d/aaa.d
mkdir -p b.d/bb.d
touch a.d/aa.d/aaa.d/baz
touch a.d/aa.d/aaa.d/bar
darcs add -v a.d/aa.d/aaa.d/bar a.d/aa.d/aaa.d/baz b.d/bb.d 2> log
test ! -s log # no output

# Make sure that darcs doesn\'t complains about duplicate adds when adding parent dirs.
mkdir c.d
touch c.d/baz
darcs add -v c.d/baz c.d 2> log
test ! -s log # no output

# Make sure that add output looks good when adding files in subdir
mkdir d.d
touch d.d/foo
darcs add -rv d.d | grep 'd.d/foo'

# 'adding a non-existent dir and file gives the expected message
not darcs add -v notadir/notafile 2>&1 | grep -i 'does not exist'

cd ..
rm -rf temp1

#  test for darcs add behaviour on missing files.

darcs init temp1
cd temp1

empty='not test -s'
nonempty='test -s'

rm -f foo
darcs add foo >stdout 2>stderr && exit 1 || true
$empty stdout
$nonempty stderr

>foo
darcs add foo >stdout 2>stderr
$nonempty stdout  # confirmation message of added file
$empty stderr

darcs add foo >stdout 2>stderr && exit 1 || true
$empty stdout
$nonempty stderr

rm foo
darcs add foo >stdout 2>stderr && exit 1 || true
$empty stdout
$nonempty stderr

cd ..
rm -rf temp1
