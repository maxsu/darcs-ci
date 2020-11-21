#!/usr/bin/env bash
# 2011, by Petr Rockai, Guillaume Hoffmann, public domain

# Tests that darcs clone --verbose reports getting a pack when there is one,
# and does not report when there is none or when --no-packs is passed.

. lib
. httplib

only-format darcs-2 # compressed repo is darcs-2

gunzip -c $TESTDATA/laziness-complete.tgz | tar xf -

cd repo

darcs optimize http
test -e _darcs/packs/basic.tar.gz
test -e _darcs/packs/patches.tar.gz
cd ..

serve_http # sets baseurl

# check that default behaviour is to get packs
rm -rf S
darcs clone $baseurl/repo S --verbose |grep "Cloning packed basic repository"

# check that it does really not get packs when --no-packs is passed
rm -rf S
darcs clone $baseurl/repo S --no-packs --verbose  |not grep "Cloning packed basic repository"

# check that it does not clam getting packs when there are not
rm -rf S
rm -rf repo/_darcs/packs/
darcs clone $baseurl/repo S --verbose |not grep "Cloning packed basic repository"
