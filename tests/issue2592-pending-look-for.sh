#!/usr/bin/env bash
. ./lib

cat << EOF > empty_pending
{
}
EOF

# darcs add a file and then rename without telling darcs

rm -rf test1
darcs init test1
cd test1
touch f
darcs add f
mv f g
# plain darcs whatsnew differs from pending
# because it sees that the file is no longer there
# (because removals are always detected implicitly)
grep 'addfile ./f' _darcs/patches/pending
not darcs whatsnew # No changes
darcs whatsnew --look-for-moves | grep 'addfile ./g'
darcs record -am 'addfile f + move f g = addfile g' --look-for-moves
# make sure pending is now empty
diff ../empty_pending _darcs/patches/pending
cd ..

# add and record a file, darcs move it and then rename it back
# without telling darcs about it

rm -rf test2
darcs init test2
cd test2
touch f
darcs add f
darcs record -lam 'addfile f' f
# make sure pending is empty
diff ../empty_pending _darcs/patches/pending
darcs move f g
darcs whatsnew | grep 'move ./f ./g'
mv g f
# plain darcs whatsnew differs from pending
# because it sees that the file is no longer there
# (because removals are always detected implicitly)
grep 'move ./f ./g' _darcs/patches/pending
darcs whatsnew | grep 'rmfile ./f'
# but with --look-for-moves pending and working cancel each other
not darcs whatsnew --look-for-moves # No changes

# so recording with --look-for-moves sees no changes
darcs record -a --look-for-moves | grep -i "you don't want to record anything"

# the record did nothing, so same checks as above should succeed
darcs whatsnew | grep 'rmfile ./f'
grep 'move ./f ./g' _darcs/patches/pending
not darcs whatsnew --look-for-moves # No changes

# # darcs add has no option --look-for-moves/replaces yet
# # so we have no way to "fix" pending
# darcs add --look-for-moves
# # pending should now be empty
# diff ../empty_pending _darcs/patches/pending
# darcs whatsnew | grep 'rmfile ./f'
# not darcs whatsnew --look-for-moves
cd ..
