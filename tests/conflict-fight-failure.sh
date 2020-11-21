#!/usr/bin/env bash
#
# Test darcs conflict fight scenario.
#
# Set up two repos RA and RB. Create conflict in RB. 
# After resolving conflict in RB, pull new patch from RA.
# Repeat, rinse.
#
# Author: Pekka Pessi
#

. ./lib

# test fails for these obsolete formats:
skip-formats darcs-1 darcs-2

# skip if time executable is not found
/usr/bin/env time --help 2> /dev/null || exit 200

num_conflicts=40

rm -rf RA RB
mkdir RA

cd RA
echo 0 > file
darcs init
darcs add file
darcs record -am0 file
cd ..

darcs get RA RB

# Create conflict in RB
cd RB
echo let it b > file
darcs record -am B
cd ..

for i in $(seq 1 $num_conflicts)
do
  cd RA
  echo Create new patch A$i in RA
  echo a$i > file
  darcs record -am A$i
  cd ..

  cd RB
  echo Pull patch A$i from RA and get a conflict

  /usr/bin/env time -f %e -o ../elapsed darcs pull ../RA --quiet --all --patch "^A$i\$" --allow-conflicts
  if (( $i == 1 )); then
    start=$(cat ../elapsed)
  else
    elapsed=$(cat ../elapsed)
    # check that the runtime is not more than quadratic in i
    (( $(echo "$elapsed < 1.5 * $i * $i * $start" | bc) ))
  fi

  echo Resolve conflict and start fighting by recording B$i
  echo let it b > file
  darcs record -am B$i
  cd ..
done
