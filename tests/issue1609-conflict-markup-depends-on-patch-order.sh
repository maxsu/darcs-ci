#!/bin/sh
rm -rf darcs.tp2
mkdir darcs.tp2
cd darcs.tp2
mkdir s1
mkdir s2
mkdir s3
cd s1; darcs init
echo -e '1\n2\n3\n4\n5\n' > foo.txt
darcs add foo.txt
darcs record --patch-name=init --author momo --skip-long-comment --all
cd ../s2; darcs init
darcs pull ../s1 --all
cd ../s3; darcs init
darcs pull ../s1 --all
cd ../s1;
echo -e "3i\nX\n.\nw\nq\n" | ed foo.txt
darcs record --patch-name=op1 --author momo --skip-long-comment --all
cd ../s2
echo -e "3d\nw\nq\n" | ed foo.txt
darcs record --patch-name=op2 --author momo --skip-long-comment --all
cd ../s3
echo -e "4i\nY\n.\nw\nq\n" | ed foo.txt
darcs record --patch-name=op3 --author momo --skip-long-comment --all
cd ../s1
darcs pull ../s2 --all
cd ../s2
darcs pull ../s1 --all
cd ../s1
darcs pull ../s3 --all
cd ../s2
darcs pull ../s3 --all
cd ../s3
darcs pull ../s1 --all
if diff ../s1/foo.txt ../s2/foo.txt; then echo 'TP2 ok S1 S2';  else echo 'TP2 ko'; fi
if diff ../s2/foo.txt ../s3/foo.txt; then echo 'TP2 ok S2 S3';  else echo 'TP2 ko'; fi
