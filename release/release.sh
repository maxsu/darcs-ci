#!/bin/sh
set -e

VERSION=`grep -i ^version darcs.cabal | rev | cut -f1 -d' ' | rev`
echo version: $VERSION

BRANCH=`echo -n $VERSION | cut -d. -f1-2`

echo branch: $BRANCH

set -x

darcs log -t $VERSION
darcs log -t $VERSION | grep -q $VERSION

runghc release/gen-version-info.hs $VERSION
tarballpath=$(cabal sdist | tail -1)
test -f $tarballpath

cd /tmp
packagename=darcs-$VERSION
rm -rf $packagename
cabal unpack $tarballpath
cd $packagename

cabal test --enable-tests --test-option="-j3"
cabal install --disable-optimisation --install-method=copy --installdir=./bin

./bin/darcs --version
./bin/darcs --version | grep -q "$VERSION (release)"
./bin/darcs --exact-version

cd ..
rm -rf $packagename
cd $wd

set +x

echo
echo ready: $tarballpath
echo
echo Next:
echo " darcs push to the public branch-$BRANCH repository"
echo " cabal upload $tarballpath"
