#!/usr/bin/env bash
## Test for issue2512 - Multiple authors in global config get overwritten

. lib
abort_windows  # different directory names on Windows

# helper
function fail {
	echo "ERROR: $1"
	exit 1
}

# add multiple author IDs to global preferences
cat >"$HOME/.darcs/author" <<-EOF
	AUTHOR_1, this one will be chosen when prompted for an author
	AUTHOR_2, this one should still be there in the end
EOF

# create a repo
rm -rf repo
mkdir repo
cd repo
darcs init

# make a change so that we have something to commit
touch changed
darcs add changed

# darcs will find multiple authors in global preferences,
# so darcs will ask for the author and we pick the first one
darcs record -am 'testing' <<-EOF
	1
EOF
echo

# check the configuration after the commit (primary test for this issue)
if ! grep -q 'AUTHOR_2' "$HOME/.darcs/author"; then
	fail "'\$HOME/.darcs/author' with multiple authors was clobbered."
fi

# in addition, confirm that the author was properly added to the repository
if ! [[ -f _darcs/prefs/author ]]; then
	fail "The author was not recorded in the repository."
fi
if ! grep -q 'AUTHOR_1' _darcs/prefs/author; then
	fail "An 'author' file was created in the repository, but does not contain the chosen author."
fi
