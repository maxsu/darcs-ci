#!/usr/bin/env bash

. lib
. httplib

# this should all work without a cache
if ! grep no-cache $HOME/.darcs/defaults; then
  echo ALL no-cache >> $HOME/.darcs/defaults
fi

rm -rf tabular
unpack_testdata tabular
serve_http # sets baseurl

rm -rf temp temp2 temp3
darcs clone --lazy $baseurl/tabular temp
darcs clone --lazy temp temp2

rm -rf temp
cd temp2
test ! -f _darcs/patches/0000005705-178beaf653578703e32346b4d68c8ee2f84aeef548633b2dafe3a5974d763bf2
darcs log -p 'Initial version' -v | cat
test -f _darcs/patches/0000005705-178beaf653578703e32346b4d68c8ee2f84aeef548633b2dafe3a5974d763bf2
cd ..

# test if we can unapply patches after a tag
rm -rf temp4
darcs clone --lazy $baseurl/tabular temp4 --tag '^0.1$'
finish_http $PWD
# and that the log -v output is correct
cd temp4
darcs log -v > LOG
grep -c 'this patch is unavailable' LOG | grep -w 32
cd ..
