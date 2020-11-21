#!/usr/bin/env bash

. lib

abort_windows # issue2590

darcs init R
cd R
touch äöüßÄÖÜ
darcs whatsnew -l | grep './äöüßÄÖÜ'
