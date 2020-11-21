#!/usr/bin/env bash

. lib
darcs init R
cd R
echo 'äöüßÄÖÜ' > file
darcs whatsnew -l --no-summary | grep '+äöüßÄÖÜ'
darcs record -lam'added file'
darcs log -v | grep '+äöüßÄÖÜ'
