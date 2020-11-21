#!/bin/sh -e
##
## Test for issue2648 - `darcs convert import` double-encodes cyrillic characters in UTF-8 input stream
##
## Copyright (C) 2020  Andrey Korobkov <alster@vinterdalen.se>
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

. lib                           # Load some portability helpers.

switch_to_utf8_locale

# Import stream into Darcs repo
cat $TESTDATA/cyrillic_import_stream | darcs convert import R

cd R

# Test patch name
darcs log | grep 'Южноэфиопский грач увёл мышь за хобот на съезд ящериц'

# Test filename and patch data
darcs annotate 'Панграмма.txt' | grep 'Широкая электрификация южных губерний даст мощный толчок подъёму сельского хозяйства'
