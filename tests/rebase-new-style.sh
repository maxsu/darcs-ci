#!/usr/bin/env bash
## Test that all operations except 'rebase upgrade' on old-style
## rebase-in-progress repos fail

. lib

rm -rf R S T log
unpack_testdata old-style-rebase
darcs init S
echo ../S > R/_darcs/prefs/defaultrepo

not darcs clone R T 2>&1 | tee log
grep 'clone a repository with an old-style rebase' log

# TODO for this test I need a tar ball with a darcs-1 repo in it
# not darcs convert darcs-2 R T 2>&1 | tee log
# grep 'old-style rebase is in progress' log

# init, help, and convert import are not an issue

IFS='
'
commands='
record
push
pull
log
diff
show contents f1
show dependencies
show files
show index
show pristine
show repo
show authors
show tags
show patch-index
amend
rebase pull
rebase apply
rebase suspend
rebase unsuspend
rebase obliterate
rebase log
rebase reify
rebase inject
rebase changes
unrecord
obliterate
tag
send
apply
optimize clean
optimize http
optimize reorder
optimize enable-patch-index
optimize disable-patch-index
optimize compress
optimize uncompress
optimize relink
optimize pristine
mark-conflicts
repair
convert export
fetch'

cd R
for cmd in $commands; do
  unset IFS
  not darcs $cmd 2>&1 | tee ../log
  grep 'old-style rebase is in progress' ../log
done
darcs rebase upgrade
darcs rebase log 2>../log
grep 'Rebase in progress: 1 suspended patch' ../log
cd ..
