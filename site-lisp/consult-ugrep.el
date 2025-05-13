;;; consult-ugrep.el --- Search with ugrep -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.3
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides ugrep integration with `consult'.
;; Ugrep is available from <https://ugrep.com>.

;;; Code:

(require 'consult)

(defcustom consult-ugrep-args
  "ugrep --null --color=never --smart-case --line-number \
--with-filename --ignore-binary --ignore-files --decompress -r -P"
  "Command line arguments for ugrep, see `consult-ugrep'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp)))
  :group 'consult)

(defun consult--ugrep-make-builder (paths)
  "Create grep command line builder given PATHS."
  (let* ((cmd (consult--build-args consult-ugrep-args)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
		   (flags (append cmd opts))
		   (type (if (or (member "-P" flags) (member "--perl-regexp" flags))
			     'pcre
			   'extended))
		   (ignore-case
		    (or (member "-i" flags) (member "--ignore-case" flags)
			(and (or (member "-S" flags) (member "--smart-case" flags))
			     (let (case-fold-search)
			       ;; Case insensitive if there are no uppercase letters
			       (not (string-match-p "[[:upper:]]" arg)))))))
	(if (or (member "-F" flags) (member "--fixed-strings" flags))
	    (cons (append cmd (list "-e" arg) opts paths)
		  (apply-partially #'consult--highlight-regexps
				   (list (regexp-quote arg)) ignore-case))
	  (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
	    (when re
	      (cons (append cmd
			    (cdr (mapcan (lambda (x) (list "--and" "-e" x)) re))
			    opts paths)
		    hl))))))))

;;;###autoload
(defun consult-ugrep (&optional dir initial)
  "Search with `ugrep' for files in DIR with INITIAL input.
See `consult-grep' for details."
  (interactive "P")
  (consult--grep  "Ugrep: " #'consult--ugrep-make-builder dir initial))

(provide 'consult-ugrep)
;;; consult-ugrep.el ends here
