# Test that we can resolve a conflict by explicitly depending on
# the conflicting patches, thereby accepting the "default" resolution
# (i.e. not to apply both) as correctly resolving the conflict.

. lib

rm -rf R S
darcs init R
cd R
touch file
darcs record -lam baseline
echo one > file
darcs record -a -m one
darcs clone . ../S
echo two > file
echo y | darcs amend -a -m two
darcs pull -a --allow-conflicts ../S
# make sure we have none of the changes:
test -z "$(cat file)"
# resolve by depending on the last two patches:
echo yyd | darcs record --ask-deps -m resolve_explicitly
darcs mark-conflicts > LOG 2>&1
grep "No conflicts" LOG
not darcs whatsnew
cd ..

# Test that an explicit dependency on a patch containing a
# partial conflict resolution does not turn it into a resolution
# of an unrelated conflict contained the same patch.

rm -rf R S
darcs init R
cd R
touch file1
touch file2
darcs record -lam baseline
echo one > file1
echo one > file2
darcs record -am one
darcs clone . ../S
echo two > file1
echo two > file2
echo y | darcs amend -a -m two
darcs pull -a --allow-conflicts ../S
# make sure we have none of the changes:
test -z "$(cat file1)"
test -z "$(cat file2)"
# resolve (only) the conflict in file1
echo three > file1
darcs record -a -m resolve_file1
# test this indeed resolves only the conflict in file1
darcs mark-conflicts > LOG 2>&1
grep 'Marking conflicts' LOG
grep 'file2' LOG
not grep 'file1' LOG
not darcs whatsnew file1
# remove the markup
darcs revert -a
# explicitly depend on the partial resolution
echo yyd | darcs record --ask-deps -m explicit
# test resolution is still only partial
darcs mark-conflicts > LOG 2>&1
grep 'Marking conflicts' LOG
grep 'file2' LOG
not grep 'file1' LOG
not darcs whatsnew file1
cd ..
