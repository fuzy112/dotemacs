;;; dch.el --- Edit debian changelog -*- lexical-binding: t -*-
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

(require 'project)
(require 'with-editor)

;;;###autoload
(defun dch ()
  "Run \"dch\" command to edit changelog."
  (interactive)
  (let* ((proj (project-current))
	 (default-directory (project-root proj))
	 (changelog-files (process-lines "find" "-name" "changelog" "-type" "f"))
	 (changelog-file (completing-read "Change log: " changelog-files))
	 (debian-dir (file-name-directory changelog-file)))
    (let ((default-directory (expand-file-name (file-name-parent-directory debian-dir))))
      (with-editor
	(start-file-process "dch" " *dch*"
			    "dch" "-U")))))

(provide 'dch)
;;; dch.el ends here
