;;; embark-bookmark.el --- Embark integration for bookmark -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: convenience

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
;;; Code:

(require 'embark)
(require 'bookmark)

(defun embark-bookmark--bmenu-bookmark ()
  "Find bookmark entry in the current line in bookmark list mode."
  (save-restriction
    (ignore-errors
      (and-let* ((bookmark (bookmark-bmenu-bookmark)))
	`(bookmark ,bookmark ,(line-beginning-position) . ,(line-end-position))))))

;;;###autoload
(defun embark-bookmark--bmenu-setup ()
  (add-hook 'embark-target-finders #'embark-bookmark--bmenu-bookmark nil t))

;;;###autoload
(add-hook 'bookmark-bmenu-mode-hook #'embark-bookmark--bmenu-setup)

(provide 'embark-bookmark)
;;; embark-bookmark.el ends here
