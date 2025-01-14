;;; outline2org.el --- Generate an Org-mode file from outlined Elisp files  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: convenience, lisp

;; This program is free software; you can redistribute it and/or modify
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

;; To export an outlined elisp file to an Org-mode buffer, run
;; `M-x outline-to-org'.

;;; Code:

(defconst outline2org-code-block-regexp "\\(?:^[^;\n].*$\\)\\(?:\\(?:^[^;\n].*$\\)\\|\n\\)+")

;;;###autoload
(defun outline-to-org ()
  "Export the current elisp buffer to an Org-mode buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (pop-to-buffer (generate-new-buffer "*org*"))
    (insert-buffer buf)
    (goto-char (point-min))
    (while (re-search-forward outline2org-code-block-regexp nil t)
      (let ((code-block (string-trim-right (match-string 0))))
        (replace-match (concat "#+begin_src elisp\n"
                               code-block
                               "\n#+end_src\n")
                       t t)))
    (goto-char (point-min))
    (when (looking-at "^;;+[[:space:]]+[^[:space:]]+[[:space:]]+---[[:space:]]+\\(.*\\)[[:space:]]+\\(?:-\\*-.*-\\*-$\\)")
      (replace-match "* \\1"))
    (goto-char (point-min))
    (while (re-search-forward "^;;\\(;+\\)" nil t)
      (replace-match (make-string (length (match-string 1)) ?*)))
    (goto-char (point-min))
    (while (re-search-forward "^;+[[:space:]]*" nil t)
      (replace-match ""))
    (org-mode)))

(provide 'outline2org)
;;; outline2org.el ends here
