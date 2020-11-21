# test for issue2639: darcs diff crashes with --last=1 and file name

. lib

darcs init R
cd R
echo a > f
cp f g
darcs record -lam 'a'
echo b > f
cp f g
darcs diff f --last=1
# issue2639: this crashed with
# ### Error applying:
# hunk ./g 1
# -a
# ### to file g:
# b
# ### Reason: Hunk wants to remove content that isn't there
cd ..
