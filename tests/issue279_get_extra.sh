#!/usr/bin/env bash

# issue279: a conflict case resulting in "bug in get_extra" with old
# format repos and "Malformed patch bundle" with darcs-2 repos.

. lib

# this test fails for darcs-1 repos
skip-formats darcs-1

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo 0 > f
darcs add f
darcs record -am 00
cd ..

for r in a b c d; do
  rm -rf temp_$r
  darcs get temp1 temp_$r
  cd temp_$r
  echo $r > f
  darcs record -am "patch:$r"
  cd ..
done

cd temp_d
darcs pull -a ../temp_a --allow-conflicts
darcs pull -a ../temp_b --allow-conflicts
darcs pull -a ../temp_c --allow-conflicts
cd ..
cd temp_c
darcs pull -a ../temp_a --allow-conflicts
darcs pull -a ../temp_b --allow-conflicts
echo rc > f
darcs record -a -m rc
cd ..
cd temp_d
darcs pull -a ../temp_c > log
not grep -i "Failed to commute common patches" log
cd ..
