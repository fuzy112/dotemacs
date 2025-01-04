;;; bookmarklet.el --- Bookmarklet creation -*- lexical-binding: t -*-
;; Copyright © 2024, 2025 Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: convenient

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

;; This file contains facility to convert arbitrary JavaScript code to
;; a bookmarklet that can be added to the bookmark tool bar of FireFox
;; (or Chromium).

;;; Code:

(require 'url-util)

(defun bookmarklet-from-string (code)
  (concat "javascript:void((function(){" (url-hexify-string code) "})())"))

;;;###autoload
(defun bookmarklet-from-buffer ()
  (interactive)
  (let ((str (bookmarklet-from-string (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer (get-buffer-create "*bookmarklet*")
      (erase-buffer)
      (insert str)
      (pop-to-buffer (current-buffer)))))

(provide 'bookmarklet)
;;; bookmarklet.el ends here
