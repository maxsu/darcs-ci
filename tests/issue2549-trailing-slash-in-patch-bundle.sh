. lib

rm -rf R S N W

mkdir R S
darcs init R
darcs init S
cd R
mkdir directory
darcs add directory
darcs record -am 'add directory'
darcs send -a -o orig.dpatch ../S
sed -e s'#adddir ./directory#adddir ./directory/#'\
 -e '/Patch bundle hash:/,+2d' orig.dpatch > tweaked.dpatch
cd ../S
darcs apply ../R/tweaked.dpatch
rm -rf directory
darcs record -am 'rm directory'
cd ..
# Now, try pulling these patches.
darcs init --no-patch-index N
cd N
darcs pull -a ../S
cd ..
darcs init --with-patch-index W
cd W
darcs pull -a ../S
cd ..
