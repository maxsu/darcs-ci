#!/usr/bin/env bash
# test that cloning a repo with not up-to-date packs still gets us all the patches

. lib
. httplib

rm -rf R
darcs init R
cd R
echo foo > foo
darcs record -lam foo
darcs tag foo
echo bar > bar
darcs record -lam bar
darcs optimize http
darcs tag "not packed"
echo qux > qux
darcs record -lam qux
cd ..

serve_http # sets baseurl

rm -rf S
darcs clone $baseurl/R S

cd S
darcs log | grep "not packed"
darcs log | grep "qux"
cd ..
