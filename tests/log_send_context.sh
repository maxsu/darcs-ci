#!/usr/bin/env bash
. ./lib

# RT#544 using context created with 8-bit chars;

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs record -la -m 'add\212 foo' | grep 'Finished record'
darcs log --context >context
date > foo
darcs record -a -m 'date foo' | grep 'Finished record'
darcs send -a -o patch --context context . | grep 'Wrote patch to'
# again with an absolute path for the context file, see issue1240
# note we must not mix \\ and / so we use /bin/pwd here
cwd=$(/bin/pwd)
darcs send -a -o patch --context "$cwd/context" . | grep 'Wrote patch to'
cd ..
