#!/usr/bin/env bash

. ./lib

# detect move of a file and then a replace in that file

test_setup() {
  num=$1
  shift
  paths=$@

  rm -rf R$num
  darcs init R$num
  cd R$num

  echo foo > old
  echo foo >> old
  echo bar >> old

  darcs record -lam 'added old'
  mv old new

  echo bar > new 
  echo bar >> new
  echo bar >> new

  darcs whatsnew --look-for-moves --look-for-replaces $paths > ../out.actual
  cd ..
}

cat <<EOF > out.expected
move ./old ./new
hunk ./new 3
-bar
+foo
replace ./new [A-Za-z_0-9] foo bar
EOF

test_setup 1 ""
diff out.actual out.expected

# same but only for old

test_setup 2 old
# remove the line about What's new in: old
sed -i '1d' out.actual
diff out.actual out.expected

# same but only for new

test_setup 3 new
# remove the line about What's new in: new
sed -i '1d' out.actual
diff out.actual out.expected

# same but only for old and new

test_setup 4 old new
# remove the line about What's new in: old new
sed -i '1d' out.actual
diff out.actual out.expected

# same but only for old and new and non-existing

test_setup 5 old new non-existing
# remove the line about What's new in: old new, and the one that reports non-existing
sed -i '1,2d' out.actual
diff out.actual out.expected
