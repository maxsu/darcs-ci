#!/usr/bin/env bash
## Test for inherit-default mechanism

. lib
rm -rf U V R1 R2 R3 S

echo 'ALL inherit-default' >> .darcs/defaults

# upstream repos
darcs init U
darcs init V

# branches of U
darcs clone U R1
darcs clone R1 R2
darcs clone R2 R3

# branches of V
darcs clone V S

not test -e U/_darcs/prefs/defaultrepo
not test -e V/_darcs/prefs/defaultrepo
test U = $(cat R1/_darcs/prefs/defaultrepo | xargs basename)
test U = $(cat R2/_darcs/prefs/defaultrepo | xargs basename)
test U = $(cat R3/_darcs/prefs/defaultrepo | xargs basename)
test V = $(cat S/_darcs/prefs/defaultrepo | xargs basename)

cd R3
for cmd in pull push send; do
  # set-default works by setting the defaultrepo of the remote repo
  darcs $cmd ../S --set-default
  test V = $(cat _darcs/prefs/defaultrepo | xargs basename)
  # but not if remote repo has no defaultrepo
  darcs $cmd ../U --set-default
  test U = $(cat _darcs/prefs/defaultrepo | xargs basename)
done
cd ..
