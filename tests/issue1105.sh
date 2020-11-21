#!/usr/bin/env bash

. lib

rm -rf temp
mkdir temp
cd temp
darcs init
darcs changes

echo changes summary > _darcs/prefs/defaults
darcs changes
echo changes summary arg > _darcs/prefs/defaults
darcs changes 2> LOG
grep 'takes no argument' LOG
echo ALL summary > _darcs/prefs/defaults
darcs changes
echo ALL summary arg > _darcs/prefs/defaults
darcs changes 2> LOG
grep 'takes no argument' LOG

echo changes last 10 > _darcs/prefs/defaults
darcs changes
echo changes last > _darcs/prefs/defaults
darcs changes 2> LOG
grep 'requires an argument' LOG
echo ALL last 10 > _darcs/prefs/defaults
darcs changes
echo ALL last > _darcs/prefs/defaults
darcs changes 2> LOG
grep 'requires an argument' LOG

echo changes author me > _darcs/prefs/defaults
darcs changes 2> LOG
grep 'has no option' LOG
echo changes author me > _darcs/prefs/defaults
darcs changes 2> LOG
grep 'has no option' LOG
echo ALL author me > _darcs/prefs/defaults
darcs changes
echo ALL unknown > _darcs/prefs/defaults
darcs changes 2> LOG
grep 'has no option' LOG

cd ..
rm -rf temp
