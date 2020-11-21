#!/usr/bin/env bash

. lib

# The git repo here deliberately contains file names with
# unusual characters in them (e.g. newline);
# the test therefore makes sense only on Posix systems,
# since Windows does not even allow to create such files.
abort_windows

git --version || exit 200 # no git installed => skip test

rm -rf gitrepo gitrepo2 darcsrepo
unpack_testdata gitrepo
cd gitrepo
git fast-export HEAD > ../git-export
git log >../git-log1
cd ..
darcs convert import darcsrepo <git-export
diff -r gitrepo/src darcsrepo/src

cd darcsrepo
darcs convert export >../darcs-export
cd ..
mkdir gitrepo2
cd gitrepo2
git init
git fast-import < ../darcs-export
git checkout master
git log > ../git-log2
cd ..
diff -I 'commit' -I 'Date:' git-log1 git-log2
diff -r gitrepo/src gitrepo2/src
