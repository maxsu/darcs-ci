#!/usr/bin/env bash
. ./lib

rm -rf temp
mkdir temp
cd temp

darcs init
cat > _darcs/prefs/defaults <<.
ALL author test
ALL ignore-times
ALL ask-deps
.

# add three depending patches for file 'a'
# expect no dependency questions
# 'q' will abort and cause future failure if an unexpected dependency is asked
touch a
darcs add a
echo q | darcs rec -am a0
darcs log -p a0 -v --machine | cat
echo 1 > a
echo q | darcs rec -am a1
darcs log -p a1 -v --machine | cat
echo 2 > a
echo q | darcs rec -am a2
darcs log -p a2 -v --machine | cat

# add some patches for file 'b'
# expect no dependency questions for file 'b',
# but every time expect questions for the three patches of file 'a'
# every 'n' should continue to ask about the next patch
# the first 'y' should make all following dependencies of 'a' implicit and stop asking
# 'q' will abort and cause future failure if an unexpected dependency is asked
touch b
darcs add b
# test 0
echo nnnY | tr '[A-Z]' '[a-z]' | darcs rec -am b0
darcs log -p b0 -v --machine | cat
# test 1
echo 1 > b
echo nnyY | tr '[A-Z]' '[a-z]' | darcs rec -am b1
darcs log -p b1 -v --machine | cat
darcs log -p b1 -v --machine | grep '\[a0'
# test 2
echo 2 > b
echo nyY | tr '[A-Z]' '[a-z]' | darcs rec -am b2
darcs log -p b2 -v --machine | grep '\[a1'
# test 3
echo 3 > b
echo yY | tr '[A-Z]' '[a-z]' | darcs rec -am b3
darcs log -p b3 -v --machine | cat
darcs log -p b3 -v --machine | grep '\[a2'

cd ..
rm -rf temp

