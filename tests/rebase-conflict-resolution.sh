#!/usr/bin/env bash

# test that we can suspend a conflict plus its resolution, then unsuspend them
# and not get any conflicts
. lib

rm -rf R S R-oneatatime R-bothtogether

darcs init R

cd R
echo 'initial content' > f
darcs rec -lam "initial content"
cd ..

darcs clone R S

cd R
echo 'content1' > f
darcs rec -am "content1"
cd ..

cd S
echo 'content2' > f
darcs rec -am "content2"
cd ..

cd R
darcs pull --allow-conflicts -a ../S
echo 'content12' > f
darcs rec -am "content12 (resolution)"

echo yyd | darcs rebase suspend
cd ..

cp -r R R-oneatatime
cp -r R R-bothtogether

cat > f.expected << END
content12
END

cd R-oneatatime
echo yd | darcs rebase unsuspend
 # get rid of conflict markers, since in this case darcs hasn't seen the resolution patch yet
darcs rev -a
echo yd | darcs rebase unsuspend

diff -u ../f.expected f
cd ..

cd R-bothtogether
echo yyd | darcs rebase unsuspend
# this time there shouldn't be any conflict markers in the first place, as the resolution was
# unsuspended at the same time
diff -u ../f.expected f
cd ..
