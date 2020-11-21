. lib

darcs init R
cd R
touch f
darcs record -l f -am'add f'
darcs rebase suspend -a --last 1
echo bla > g
darcs add g
darcs revert -a
not grep 'DO NOT TOUCH' _darcs/patches/unrevert
