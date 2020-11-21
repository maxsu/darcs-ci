#!/usr/bin/env bash
. ./lib

# For issue855: wish: avoid taking lock if using --dry-run
chmod -R u+w temp2 || :
rm -rf temp1 temp2
darcs init temp1
darcs init temp2
cd temp2
touch x
darcs record -lam test
cd ..
chmod -R u-w temp2
cd temp2
# need to capture this failure so that we can still
# chmod -R u+w the directory even if we fail
darcsexit=0
darcs push --dry-run ../temp1 || darcsexit=$?
cd ..
chmod -R u+w temp2 # so that other scripts can cleanup
if [ $darcsexit -ne 0 ]; then
  exit $darcsexit
fi
