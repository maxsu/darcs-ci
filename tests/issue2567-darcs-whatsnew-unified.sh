#!/usr/bin/env bash

# check that darcs whatsnew --unified outputs correct context lines

. lib

cat > before << EOF
1
2
3
4
5
EOF

cat > after << EOF
1
3
4a
4b
5
EOF

cat > exp << EOF
hunk ./file 2
 1
-2
 3
hunk ./file 3
-4
+4a
+4b
 5
EOF

darcs init R
cd R
cp ../before file
darcs record -lam 'add file'
cp ../after file
darcs whatsnew --unified > ../got
diff ../exp ../got
