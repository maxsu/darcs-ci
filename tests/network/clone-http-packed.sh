#!/usr/bin/env bash
# Written in 2010 by Petr Rockai, placed in public domain

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
rm -rf S
darcs clone --packs $baseurl/repo S
cd S
rm _darcs/prefs/sources # avoid any further contact with the original repository
darcs check
