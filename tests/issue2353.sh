## issue2353: darcs does not show old changes of moved files

#!/usr/bin/env bash
. ./lib

rm -rf test
mkdir test
cd test
darcs init
echo 'hello world' > file
darcs add file
darcs record -am 'patch 1' --skip-long
darcs mv file new_file
darcs record -am 'file -> new_file' --skip-long
darcs diff -p 'patch 1' | grep -F "+hello world"
cd ..
