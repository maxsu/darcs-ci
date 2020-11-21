#!/usr/bin/env bash
. lib

rm -rf temp1
darcs init temp1
cd temp1
mkdir a b
touch a/a b/b
darcs record -lam ab
darcs annotate a/a
echo x > c
darcs record -lam foo -A 'Mark Stosberg <a@b.com>'
darcs annotate c
darcs annotate c | grep "a@b.com"
cd ..

# issue1473 annotate repodir
rm -rf temp1
mkdir temp1
cd temp1
darcs init
mkdir a b
touch a/a b/b
darcs add --rec .
darcs record -a -m ab -A test
darcs annotate a/a
darcs annotate . > inner
# annotate --repodir=something '.' should work
cd ..
darcs annotate --repodir temp1 '.' > temp1/outer
cd temp1
diff inner outer

cd ..

# issue2207 : annotate on directories
rm -rf temp1
darcs init temp1
cd temp1
mkdir d
touch d/f
darcs record -lam 'p1'
darcs annotate d | grep 'p1'
cd ..

# issue1473 - check that annotate works with and without
# repodir and with "." argument.  It should fail with the empty string as
# a single argument and without any arguments.

rm -rf temp1
darcs init temp1
cd temp1
echo 'Example content.' > f
darcs record -lam 'Added f.'
darcs annotate .
darcs annotate f
not darcs annotate
not darcs annotate ''

cd ..
darcs annotate --repodir=temp1 .
darcs annotate --repodir=temp1 f
not darcs annotate --repodir=temp1
not darcs annotate --repodir=temp1 ''
