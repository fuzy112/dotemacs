#!/usr/bin/env bash
# Configure 'git diff' hunk header format.

# This xfuncname is based on Git's built-in 'cpp' pattern.
# The first line rejects jump targets and access declarations.
# The second line matches top-level functions and methods.
# The third line matches preprocessor and DEFUN macros.
git config diff.cpp.xfuncname \
'!^[ \t]*[A-Za-z_][A-Za-z_0-9]*:[[:space:]]*($|/[/*])
^((::[[:space:]]*)?[A-Za-z_][A-Za-z_0-9]*[[:space:]]*\(.*)$
^((#define[[:space:]]|DEFUN).*)$'
git config diff.elisp.xfuncname \
    '^\([^[:space:]]*(def[^[:space:]]+[[:space:]]+([^()[:space:]]+)|setup[[:space:]]+([^[:space:]]+)|gptel-make-.*)'
git config 'diff.m4.xfuncname' '^((m4_)?define|A._DEFUN(_ONCE)?)\([^),]*'
git config 'diff.make.xfuncname' \
	   '^([$.[:alnum:]_].*:|[[:alnum:]_]+[[:space:]]*([*:+]?[:?]?|!?)=|define .*)'
git config 'diff.shell.xfuncname' \
	   '^([[:space:]]*[[:alpha:]_][[:alnum:]_]*[[:space:]]*\(\)|[[:alpha:]_][[:alnum:]_]*=)'
git config diff.texinfo.xfuncname \
	   '^@node[[:space:]]+([^,[:space:]][^,]+)'

git config diff.gpg.textconv 'gpg -d --quiet --yes --compress-algo=none --no-encrypt-to --batch --use-agent'
git config diff.gpg.binary true
git config commit.gpgSign true

cat >.git/hooks/pre-push <<-"EOF"
#!/bin/sh

# Exit immediately if a command exits with a non-zero status.
set -e

while read local_ref local_sha remote_ref remote_sha; do
  # Skip if this is a delete operation (newrev is 0)
  if [[ "$remote_sha" == "0000000000000000000000000000000000000000" ]]; then
    continue
  fi

  # Find all commits that are in the local ref but not in the remote ref
  for sha in $(git rev-list $remote_sha..$local_sha); do
    # Get the signature status of the commit
    sig_status=$(git verify-commit $sha 2>&1 || true)

    # Check if the commit is signed and verified
    if ! echo "$sig_status" | grep -q "Good signature"; then
      echo "Error: Commit $sha is not properly signed."
      echo "$sig_status"
      exit 1
    fi
  done
done
EOF
chmod +x .git/hooks/pre-push

cat >.git/hooks/post-receive <<-"EOF"
set -e

while read old_sha new_sha ref_name; do
  # Skip if this is a delete operation (new_sha is 0)
  if [[ "$new_sha" == "0000000000000000000000000000000000000000" ]]; then
    continue
  fi

  # If old_sha is 0 (creating a new branch), check all commits in the new branch
  if [[ "$old_sha" == "0000000000000000000000000000000000000000" ]]; then
    commits=$(git rev-list $new_sha)
  else
    # Otherwise, only check the commits being added
    commits=$(git rev-list $old_sha..$new_sha)
  fi

  for sha in $commits; do
    # Use git verify-commit to check the signature
    # The || true prevents the script from exiting on the first non-signed commit
    output=$(git verify-commit $sha 2>&1 || true)

    # Check if the output contains "Good signature"
    if ! echo "$output" | grep -q "Good signature"; then
      echo "Error: Commit $sha on $ref_name is not properly signed."
      echo "$output"
      exit 1 # Reject the entire push
    fi
  done
done
EOF
chmod +x .git/hooks/post-receive
