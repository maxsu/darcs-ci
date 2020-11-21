#!/usr/bin/env bash

## test that we cleanly fail with malformed numbers
## and ranges for --max-count, --last, and --index

. ./lib

check_arg_parse_error() {
  not darcs log $1 2>LOG 1>&2
  not grep -i bug LOG
  grep -i "cannot parse" LOG
}

darcs init R
cd R
check_arg_parse_error --max-count=-1
check_arg_parse_error --max-count=x
check_arg_parse_error --last=-1
check_arg_parse_error --last=x
# note zero is not a valid index
check_arg_parse_error --index=-1
check_arg_parse_error --index=0
check_arg_parse_error --index=x
check_arg_parse_error --index=-1-2
check_arg_parse_error --index=1--2
check_arg_parse_error --index=0-1
check_arg_parse_error --index=1-0
check_arg_parse_error --index=x-y
# but indexes and counts may exceed number of patches
darcs log --max-count=1
darcs log --last=1
darcs log --index=1
darcs log --index=1-2
cd ..
