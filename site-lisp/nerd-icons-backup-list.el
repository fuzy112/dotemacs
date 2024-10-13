;;; nerd-icons-backup-list.el --- Nerd icons support for backup-list-mode -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: backup, files, icon

;; This file is not part of GNU Emacs.
;; This file is not part of nerd-icons.

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

;; This package adds nerd-icons support for backup-list-mode buffers.

;;; Code:

(require 'backup)
(require 'nerd-icons)

;;;###autoload
(defun nerd-icons-format-backup-list-entry (file backup-file)
  (let* ((orig-str (backup-list-default-format-entry-function file backup-file))
	 (props (text-properties-at 0 orig-str))
	 (str (concat "  "
		      (nerd-icons-icon-for-file file)
		      " "
		      (string-trim-left orig-str))))
    (set-text-properties 0 (length str) props str)
    str))

;;;###autoload
(setq backup-list-format-entry-function #'nerd-icons-format-backup-list-entry)

(provide 'nerd-icons-backup-list)
;;; nerd-icons-backup-list.el ends here
