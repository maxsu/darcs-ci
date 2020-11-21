#!/usr/bin/env bash
## Test for issue2275 - darcs follows symbolic links instead of properly
## ignoring them.
## When substituting a recorded file with a symbolic link, darcs becomes
## confused and associates the filename label to the content of the file
## pointed by the link.
##
## Copyright (C) 2017 Gian Piero Carrubba
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib
abort_windows                   # Skip test on Windows

rm -rf R
darcs init R
cd R
touch g
echo 'This line should not appear in g.' > f
darcs record -lam 'Add f and g.'
rm -f g                         # Remove g and create a link with the
ln -s f g                       # same name ponting to f
darcs diff g | not grep -F '+This line should not appear in g.'
cd ..

# extended test extracted from the bug report

extended_test () {

  rm -rf R log
  mkdir log

  # initialize the repository
  darcs init R

  cd R
  echo ImmutableFile > file
  darcs rec -lam init

  # add a test file and record the patch
  echo TemporaryFile > maybeFile
  darcs rec -lam 'Add maybeFile'

  # remove the just added file and check that darcs recognizes the
  # removal
  rm maybeFile
  darcs wh -s > ../log/before-ln-wh-s
  darcs wh -ls > ../log/before-ln-wh-ls

  diff -u ../log/before-ln-wh-s ../log/before-ln-wh-ls

  # create a symbolic link with the same name of the just removed
  # file pointing to an existent file (does not need to be in the
  # repodir)
  ln -s file maybeFile

  # now you get different opinions about what changed depending on
  # the use of the '--look-for-adds' option. In both case 'maybeFile'
  # is wrongly reported as changed (the current content of 'maybeFile'
  # is assumed to be the one of the file the link points to), anyway
  # using the '-l' option you also get the right information that
  # 'maybeFile' has been removed.
  darcs wh -s > ../log/after-ln-wh-s
  darcs wh -ls > ../log/after-ln-wh-ls

  diff -u ../log/before-ln-wh-s ../log/after-ln-wh-s
  diff -u ../log/before-ln-wh-ls ../log/after-ln-wh-ls

  # trying to record the changes without the use of '-l' leads to
  # the wrong patch being recorded
  darcs rec -am 'Maybe remove maybeFile'
  darcs log --last 1 -s | not grep 'M ./maybeFile'

  # unrecord the wrong patch
  darcs unrec --last 1 -a

  # passing '-l' to the record command leads to the right patch
  # being  recorded.
  darcs rec -lam 'Remove maybeFile'
  darcs log --last 1 -s | grep -F './maybeFile' > ../log/after-record-l

  diff -u -w ../log/after-record-l ../log/after-ln-wh-ls

  # now if you unrecord the last patch the `-l' option does not
  # make a difference anymore
  darcs unrec --last 1 -a
  darcs wh -s > ../log/after-unrec-wh-s
  darcs wh -ls > ../log/after-unrec-wh-ls

  # use -w to ignore different indentation
  diff -u -w ../log/after-record-l ../log/after-unrec-wh-s
  diff -u -w ../log/after-record-l ../log/after-unrec-wh-ls

  # create again the issue, this time giving the file the same
  # content as the file referenced by the link
  echo ImmutableFile > maybeFileThree
  darcs rec -lam 'Add maybeFileThree'
  rm maybeFileThree

  darcs wh -s > ../log/before-ln-wh-s
  darcs wh -ls > ../log/before-ln-wh-ls

  ln -s file maybeFileThree

  darcs wh -s > ../log/after-ln-wh-s
  darcs wh -ls > ../log/after-ln-wh-ls

  diff -u ../log/before-ln-wh-s ../log/after-ln-wh-s
  diff -u ../log/before-ln-wh-ls ../log/after-ln-wh-ls

  darcs rec -lam 'Remove maybeFileThree'

  # create again the problem, pointing the symbolic link to a
  # not-existent file
  echo JustAnotherTemporaryFile > maybeFileFour
  darcs rec -lam 'Add maybeFileFour'
  rm maybeFileFour

  darcs wh -s > ../log/before-ln-wh-s
  darcs wh -ls > ../log/before-ln-wh-ls

  ln -s not-existent maybeFileFour

  darcs wh -s > ../log/after-ln-wh-s
  darcs wh -ls > ../log/after-ln-wh-ls

  diff -u ../log/before-ln-wh-s ../log/after-ln-wh-s
  diff -u ../log/before-ln-wh-ls ../log/after-ln-wh-ls

  darcs rec -lam 'Remove maybeFileFour'

  cd ..
}

echo ############## --ignore-times #################

extended_test

echo ############ --no-ignore-times ################

restore_defaults () {
  sed -i 's/no-ignore-times/ignore-times/g' $1/.darcs/defaults
}
sed -i 's/ignore-times/no-ignore-times/g' .darcs/defaults

trap "restore_defaults '$PWD'" EXIT
extended_test
