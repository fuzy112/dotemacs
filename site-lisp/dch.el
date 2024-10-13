;;; dch.el --- Edit debian changelog -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: convenience

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
