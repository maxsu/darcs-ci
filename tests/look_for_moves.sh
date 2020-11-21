#!/usr/bin/env bash

. ./lib

rm -rf temp1
mkdir temp1
cd temp1

# simple add and move

darcs init
touch foo
darcs record -lam add_file
mv foo foo2
darcs wh --summary --look-for-moves >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
EOF
diff -u log log.expected
rm log log.expected
darcs record -am move_file --look-for-moves
darcs wh --look-for-moves --look-for-adds  >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# simple add and move dir

darcs init
mkdir foo
darcs record -lam add_dir
mv foo foo2
darcs wh --summary --look-for-moves >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
EOF
diff -u log log.expected
rm log log.expected
darcs record -am move_dir --look-for-moves
darcs wh --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# add, move and add same name

darcs init
touch foo
darcs record -lam add_file
mv foo foo2
touch foo
darcs wh --summary --look-for-moves >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
EOF
diff -u log log.expected
rm log log.expected
darcs wh --summary --look-for-moves --look-for-adds >log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo2
a ./foo
EOF
darcs record -am move_file_add_file --look-for-moves --look-for-adds
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# amend-record

darcs init
touch foo
darcs record -lam add_file
mv foo foo2
echo 'yyy' | darcs amend-record -p add_file --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
darcs log -v --machine -p add_file >log 2>&1
# 'darcs log --machine' internally calls 'showPatch ForStorage'
# which is why we need to distinguish between darcs-3 and earlier here.
# This behavior of 'darcs log' is questionable at best.
if test "$format" = darcs-3; then
  grep -A1 "] hash 1 " log | grep "addfile ./foo2"
else
  grep "] addfile ./foo2" log
fi
rm -rf *

# add, move, add same name and amend-record

darcs init
touch foo
darcs record -lam add_file
mv foo foo2
touch foo
echo 'yyyy' | darcs amend-record -p add_file --look-for-moves --look-for-adds
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
darcs log -v --patch add_file >log 2>&1
grep "addfile ./foo" log
grep "addfile ./foo2" log
rm -rf *

# add, move, amend-record, move, amend-record

darcs init
touch foo
darcs record -lam add_file
mv foo foo2
echo 'yyy' | darcs amend-record -p add_file --look-for-moves
mv foo2 foo
echo 'yyy' | darcs amend-record -p add_file --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# move dir with content

darcs init
touch foo # created before dir to get a lower inode
mkdir dir
mv foo dir
darcs record -lam add_files
mv dir dir2
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
 ./dir -> ./dir2
EOF
diff -u log log.expected
rm log log.expected
darcs record -am move_dir --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# name swapping -- unsupported thus ignored

darcs init
touch foo foo2
darcs record -lam add_file
mv foo foo.tmp
mv foo2 foo
mv foo.tmp foo2
not darcs wh --look-for-moves
rm -rf *

# dir swapping -- dir moves are ignored but inner files moves are considered

darcs init
mkdir dir dir2
touch dir/foo dir2/foo2
darcs record -lam add_files_and_dirs
mv dir dir.tmp
mv dir2 dir
mv dir.tmp dir2
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
 ./dir/foo -> ./dir2/foo
 ./dir2/foo2 -> ./dir/foo2
EOF
diff -u log log.expected
rm -rf *

# darcs mv before a plain mv

darcs init
touch foo
darcs record -lam add_files_and_dirs
darcs mv foo foo2
mv foo2 foo3
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
 ./foo -> ./foo3
EOF
diff -u log log.expected
rm log log.expected
darcs record -a -m move_dirs --look-for-moves
darcs wh --look-for-moves --look-for-adds >log 2>&1
grep -vE "(^ *$|^\+|No changes!)" log
rm -rf *

# mv to a boring filename

darcs init
touch foo
darcs record -lam add_files_and_dirs
mv foo foo~
darcs wh --summary --look-for-moves > log 2>&1
cat > log.expected <<EOF
R ./foo
EOF
diff -u log log.expected
rm -rf *

cd ..
rm -rf temp1
