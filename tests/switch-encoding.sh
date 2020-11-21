. lib

switch_to_utf8_locale
lc_utf8=$LC_ALL

darcs init E
darcs clone E U

cd U
mkdir Texte
echo 'Müßiggang' > 'Texte/böse Zeichen'
darcs record -lam 'Erste Änderung'
darcs log -v
darcs send -ao bundle
tail -n+7 bundle > u_bundle

LC_ALL=C

darcs log -v
darcs send -ao bundle
tail -n+7 bundle > c_bundle

diff u_bundle c_bundle

darcs apply u_bundle | grep 'already .*applied'
darcs obliterate -a
darcs apply u_bundle | grep 'Finished applying'
darcs unrecord -a
darcs revert -a
darcs apply u_bundle | grep 'Finished applying'


LC_ALL=$lc_utf8

darcs apply c_bundle | grep 'already .*applied'
darcs obliterate -a
darcs apply c_bundle | grep 'Finished applying'
darcs unrecord -a
darcs revert -a
darcs apply c_bundle | grep 'Finished applying'

LC_ALL=C

cd ..

darcs clone U C
diff -r U/Texte C/Texte

cd C
darcs pull ../E --set-default

darcs apply ../U/u_bundle | grep 'already .*applied'
diff -r ../U/Texte Texte
darcs obliterate -ao ou_bundle
diff ../U/u_bundle ou_bundle
darcs apply ou_bundle | grep 'Finished applying'
diff -r ../U/Texte Texte
darcs unrecord -a
darcs revert -a
darcs pull ../U -a
diff -r ../U/Texte Texte
darcs send -ao bundle
tail -n+7 bundle > c_bundle

LC_ALL=$lc_utf8

darcs send -ao bundle
tail -n+7 bundle > u_bundle
diff u_bundle c_bundle

darcs apply c_bundle | grep 'already .*applied'
diff -r ../U/Texte Texte
darcs obliterate -ao oc_bundle
diff c_bundle oc_bundle
darcs apply c_bundle | grep 'Finished applying'
diff -r ../U/Texte Texte
darcs unrecord -a
darcs revert -a
darcs pull ../U -a
diff -r ../U/Texte Texte
darcs send -ao bundle
tail -n+7 bundle > c_bundle

cd ..

diff U/u_bundle C/u_bundle
diff -r U/Texte C/Texte
