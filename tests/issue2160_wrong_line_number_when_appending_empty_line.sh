# Off-by-one error when reporting patches that append an empty line to a file.
# An affected darcs would report 'hunk ./f 3' in the provided tests.
. lib

darcs init R
cd R
echo 'first line' > f
darcs record -lam 'first line'
echo '' >> f
darcs whatsnew > wh
darcs record -am 'appended empty line'
darcs log -v --last=1 > log
darcs annotate f > ann

# these currently all fail:
grep 'hunk ./f 2' wh
grep 'hunk ./f 2' log
not grep unknown ann
grep '#2' ann
