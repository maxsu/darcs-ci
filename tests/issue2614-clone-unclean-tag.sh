. lib

rm -rf R S T
darcs init R
cd R
echo apply allow-conflicts >> _darcs/prefs/defaults
echo bla > foo
darcs record -lam 'bla R'
darcs tag one
cd ..

darcs init S
cd S
echo bla > foo
darcs record -lam 'bla S'
darcs tag one
echo blub > foo
darcs record -lam 'blub S'
darcs tag two
darcs push ../R -a
cd ..

darcs clone R T --tag two
