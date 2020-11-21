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
echo q | darcs record added not-added recorded removed not-existing /not-repo-path > ../record.out 2>&1

check_report_yes ../record.out 'invalid repository path' /not-repo-path
check_report_no ../record.out 'invalid repository path' added not-added recorded removed not-existing

check_report_yes ../record.out 'non-existing' not-added not-existing
check_report_no ../record.out 'non-existing' added recorded removed /not-repo-path

check_report_yes ../record.out 'not.\+in.\+repository' added
check_report_no ../record.out 'not.\+in.\+repository' not-added recorded removed not-existing /not-repo-path

check_report_yes ../record.out 'Recording' added recorded removed
check_report_no ../record.out 'Recording' not-added not-existing /not-repo-path

echo q | darcs record -l added not-added recorded removed not-existing /not-repo-path > ../record-l.out 2>&1

check_report_yes ../record-l.out 'invalid repository path' /not-repo-path
check_report_no ../record-l.out 'invalid repository path' added not-added recorded removed not-existing

check_report_yes ../record-l.out 'non-existing' not-existing
check_report_no ../record-l.out 'non-existing' added not-added recorded removed /not-repo-path

check_report_no ../record-l.out 'not.\+in.\+repository' added not-added recorded removed not-existing /not-repo-path

check_report_yes ../record-l.out 'Recording' added not-added recorded removed
check_report_no ../record-l.out 'Recording' not-existing /not-repo-path

echo q | darcs record -l -q added not-added recorded removed not-existing /not-repo-path > ../record-l-q.stdout

not grep 'paths' ../record-l-q.stdout
check_report_no ../record-l-q.stdout 'Recording' added not-added recorded removed not-existing /not-repo-path

echo q | darcs record -q -a -m'patchname' added not-added recorded removed not-existing /not-repo-path > ../record-q-a.stdout

not grep '.' ../record-q-a.stdout
