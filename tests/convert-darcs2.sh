#!/usr/bin/env bash

## Tests for convert command based on previously checked results
## to generate new test material for this test,
## see bin/convert-writer.sh
##
## Copyright (C) 2009 Ganesh Sittampalam
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib

only-format darcs-2

runtest() {
    opt=$1
    name=$2
    rm -rf $opt/$name
    mkdir -p $opt/$name
    cd $opt/$name

    mkdir repo
    cd repo
    darcs init --darcs-1
    darcs apply --allow-conflicts $TESTDATA/convert/darcs1/$name.dpatch
    cd ..
    echo 'yes' | darcs convert darcs-2 repo repo2 --$opt
    mkdir empty-darcs2
    cd empty-darcs2
    darcs init --darcs-2
    cd ..
    cd repo2
    darcs send --no-minimize -a -o ../$name-darcs2.dpatch ../empty-darcs2
    cd ..
    compare_bundles $TESTDATA/convert/darcs2/$name.dpatch $name-darcs2.dpatch

    cd ../..
}

for opt in no-working-dir with-working-dir; do
    runtest $opt simple
    runtest $opt twowayconflict
    runtest $opt threewayconflict
    runtest $opt threewayanddep
    runtest $opt threewayandmultideps
    runtest $opt resolution
    runtest $opt tworesolutions
done
