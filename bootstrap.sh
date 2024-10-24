#!/bin/bash

# Find the path of the script itself
dotemacsPath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd "$dotemacsPath" || exit 1

find -name "*.elc" -delete
rm -f package-quickstart.el
rm -f site-lisp-quickstart.el

emacs -batch -l init.el -f package-recompile-all
emacs -batch -l init.el -f site-lisp-byte-compile-all