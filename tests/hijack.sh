#!/usr/bin/env bash

# Testing patch hijack interactions

. lib

rm -rf temp1
# set up the repository
mkdir temp1
cd temp1
darcs init
cd ..

# create some simple patches by somebody else
cd temp1
touch 1 2 3 4 5
darcs add *
darcs record -a --author you 1 -m 'patch1a'
darcs record -a --author you 2 -m 'patch2a'
darcs record -a --author you 3 -m 'patch3a'
darcs record -a --author you 4 -m 'patch4a'
darcs record -a --author you 5 -m 'patch5a'
cd ..

# try amending a patch
cd temp1
echo yn | darcs amend -p patch5 -m 'patch5b' | grep "Amend anyway"
darcs log | grep patch5
darcs log | not grep patch5b
echo yy | darcs amend -p patch5 -m 'patch5b' | grep "Amend anyway"
darcs log | grep patch5b
cd ..

# try some unsuspending
cd temp1
# ...hijack one
# abort everywhere: need selectchanges for smarter behaviour
echo yyn  | darcs rebase suspend   --all
darcs log | grep 'patch1a'  # nothing suspended
darcs log | grep 'patch5b'  # nothing suspended
not darcs rebase unsuspend --all # not in progress
# ...hijack all
echo ya   | darcs rebase suspend   --all
darcs log | not grep 'patch1a'
darcs log | not grep 'patch5b'
darcs rebase unsuspend --all
darcs log |     grep 'patch1a'
darcs log |     grep 'patch5b'
cd ..

# make some conflicting patches in another repo
mkdir temp2
cd temp2
darcs init
touch 1 2 3 4 5
darcs add *
darcs record -a --author thirdperson 1 -m 'patch1c'
darcs record -a --author thirdperson 2 -m 'patch2c'
darcs record -a --author thirdperson 3 -m 'patch3c'
darcs record -a --author thirdperson 4 -m 'patch4c'
darcs record -a --author thirdperson 5 -m 'patch5c'
cd ..

# try suspending via rebase pull
cd temp1
# first 'y' is for a "repositories seem to be unrelated" prompt
echo yayyn | darcs rebase pull ../temp2 --all
darcs log | grep 'patch1a'  # nothing suspended or pulled
darcs log | grep 'patch5b'  # nothing suspended or pulled
not darcs rebase unsuspend --all # not in progress

echo yayyyyyy | darcs rebase pull ../temp2 --all
darcs log | not grep 'patch1a'
darcs log | not grep 'patch5b'

darcs obliterate -a -p 'patch1c'
darcs obliterate -a -p 'patch2c'
darcs obliterate -a -p 'patch3c'
darcs obliterate -a -p 'patch4c'
darcs obliterate -a -p 'patch5c'

darcs rebase unsuspend --all
darcs log |     grep 'patch1a'
darcs log |     grep 'patch5b'
cd ..

# try suspending via rebase apply

cd temp2
darcs obliterate -a -o c.dpatch
cd ..

cd temp1
echo ayyn | darcs rebase apply ../temp2/c.dpatch --all
darcs log | grep 'patch1a'  # nothing suspended or pulled
darcs log | grep 'patch5b'  # nothing suspended or pulled
not darcs rebase unsuspend --all # not in progress

echo ayyyyyy | darcs rebase apply ../temp2/c.dpatch --all
darcs log | not grep 'patch1a'
darcs log | not grep 'patch5b'

darcs obliterate -a -p 'patch1c'
darcs obliterate -a -p 'patch2c'
darcs obliterate -a -p 'patch3c'
darcs obliterate -a -p 'patch4c'
darcs obliterate -a -p 'patch5c'

darcs rebase unsuspend --all
darcs log |     grep 'patch1a'
darcs log |     grep 'patch5b'
cd ..
