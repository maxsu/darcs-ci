#!/bin/sh -e

echo Checks that dependencies between Darcs modules don\'t violate layering constraints
echo Output below should not contain any \'import\' statements:
echo
find src/Darcs/Util -type f | xargs grep Darcs.Patch
find src/Darcs/Util -type f | xargs grep Darcs.Repository
find src/Darcs/Util -type f | xargs grep Darcs.UI
find src/Darcs/Patch -type f | xargs grep Darcs.Repository
find src/Darcs/Patch -type f | xargs grep Darcs.UI
find src/Darcs/Repository -type f | xargs grep Darcs.UI
