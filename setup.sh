#!/bin/bash
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
           '^\([^[:space:]]*(def[^[:space:]]+[[:space:]]+([^()[:space:]]+)|setup[[:space:]]+([^[:space:]]+))'
git config 'diff.m4.xfuncname' '^((m4_)?define|A._DEFUN(_ONCE)?)\([^),]*'
git config 'diff.make.xfuncname' \
	   '^([$.[:alnum:]_].*:|[[:alnum:]_]+[[:space:]]*([*:+]?[:?]?|!?)=|define .*)'
git config 'diff.shell.xfuncname' \
	   '^([[:space:]]*[[:alpha:]_][[:alnum:]_]*[[:space:]]*\(\)|[[:alpha:]_][[:alnum:]_]*=)'
git config diff.texinfo.xfuncname \
	   '^@node[[:space:]]+([^,[:space:]][^,]+)'
