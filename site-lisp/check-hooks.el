;;; check-hooks.el --- Check usage of hooks -*- lexical-binding: t -*-
;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

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

;; This package provides a command to detect premature hook additions
;; in Emacs Lisp buffers.

;; Hooks can have standard default values that are set when the hook
;; is defined.  However, if `add-hook' is called before the hook
;; variable is defined, these standard values will never take effect
;; because the hook variable will already contain the added function
;; when it gets defined.

;; Usage:
;;
;; M-x check-hooks-in-buffer
;;
;; This command scans the current buffer for `add-hook' calls and
;; checks if the referenced hook variables have standard values defined
;; in files that haven't been preloaded.  If such hooks are found, you
;; will be prompted to enter a recursive edit for inspection, allowing
;; you to examine the hook's context before proceeding.

;;; Code:

(defun check-hooks--file-preloaded-p (filename)
  (let ((filename-nosuffix (file-name-sans-extension filename)))
    (seq-some (lambda (file)
                (string-suffix-p file filename-nosuffix))
              preloaded-file-list)))

(defun check-hooks-in-buffer ()
  "Check the usage of `add-hook' in the current buffer.
For each found hook, if the file defining the hook has not been
preloaded and the hook has a standard value, prompt the user whether to
enter a recursive edit for inspection. This allows examining the hook's
context before proceeding with the buffer analysis."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^(add-hook +'\\_<\\(\\(?:\\s_\\|\\w\\)+\\)\\_>.*" nil t)
      (when-let* ((line-start (match-beginning 0))
                  (line (match-string 0))
                  (hook (intern (match-string 1)))
                  (standard-value (custom--standard-value hook))
                  (file (symbol-file hook 'defvar)))
        (and (not (check-hooks--file-preloaded-p file))
             (yes-or-no-p (format "Hook `%S' has a standard value.  Enter recursive-edit? "
                                  hook))
             (recursive-edit))))))

(provide 'check-hooks)
;;; check-hooks.el ends here
