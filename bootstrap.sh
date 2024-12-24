#!/bin/bash

# Find the path of the script itself
dotemacsPath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd "$dotemacsPath" || exit 1

find -name "*.elc" -delete
rm -f package-quickstart.el
rm -f site-lisp-quickstart.el

emacs -batch -l early-init.el -l init.el \
      -e straight-pull-recipe-repositories \
      -e straight-thaw-versions \
      -e straight-check-all
