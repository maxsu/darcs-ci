#!/usr/bin/env bash

# Load some portability helpers
. lib

echo added          >> ../all_paths
echo not-added      >> ../all_paths
echo recorded       >> ../all_paths
echo removed        >> ../all_paths
echo not-existing   >> ../all_paths
echo /not-repo-path >> ../all_paths

check_report() {
  yes="$1"
  log="$2"
  pre="$3"
  shift
  shift
  shift
  paths="$@"
  for arg in "$@"; do
    grep $yes "$pre.\+$arg" "$log"
  done
}

check_report_yes() {
  check_report '' "$@"
}

check_report_no() {
  check_report '-v' "$@"
}

# Create and populate test repo
darcs init --repo R
cd R
touch recorded removed
darcs add recorded removed
darcs record -am'two files'
touch added not-added
rm removed
darcs add added

# Now do the tests
darcs whatsnew added not-added recorded removed not-existing /not-repo-path > ../whatsnew.out 2>&1

check_report_yes ../whatsnew.out 'invalid repository path' /not-repo-path
check_report_no ../whatsnew.out 'invalid repository path' added not-added recorded removed not-existing

check_report_yes ../whatsnew.out 'non-existing' not-added not-existing
check_report_no ../whatsnew.out 'non-existing' added recorded removed /not-repo-path

check_report_yes ../whatsnew.out "What's new" added recorded removed
check_report_no ../whatsnew.out "What's new" not-added not-existing /not-repo-path

darcs whatsnew -l added not-added recorded removed not-existing /not-repo-path > ../whatsnew-l.out 2>&1

check_report_yes ../whatsnew-l.out 'invalid repository path' /not-repo-path
check_report_no ../whatsnew-l.out 'invalid repository path' added not-added recorded removed not-existing

check_report_yes ../whatsnew-l.out 'non-existing' not-existing
check_report_no ../whatsnew-l.out 'non-existing' added not-added recorded removed /not-repo-path

check_report_no ../whatsnew-l.out 'not.\+in.\+repository' added not-added recorded removed not-existing /not-repo-path

check_report_yes ../whatsnew-l.out "What's new" added not-added recorded removed
check_report_no ../whatsnew-l.out "What's new" not-existing /not-repo-path

darcs whatsnew -l -q added not-added recorded removed not-existing /not-repo-path > ../whatsnew-l-q.stdout

not grep 'paths' ../whatsnew-l-q.stdout
check_report_no ../whatsnew-l-q.stdout "What's new" added not-added recorded removed not-existing /not-repo-path
