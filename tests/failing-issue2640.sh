# issue 2640 - rollback with filename may fail to select any patches

. lib

rm -rf R
darcs init R
cd R
echo text > file1
echo text > file2
darcs record -lam file1and2
echo othertext > file2
darcs record -am onlyfile2
darcs rollback -a file1 | tee LOG
not grep -i 'No patches' LOG
darcs whatsnew | tee LOG
grep text LOG
cd ..
