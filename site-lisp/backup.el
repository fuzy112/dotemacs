;;; backup.el --- Backup management                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Zhengyi Fu

;; Package-Version: 0.7.3
;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: convenience

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

;; This package facilitates the management and interaction with file
;; backups in Emacs.  It offers functionality to:
;;
;; - List all backup files associated with a particular file.
;; - Jump to and view a specific backup file.
;; - Perform diffs between a backup file and original file to compare
;; changes.
;; - Restore the original file from a selected backup file.

;;; Code:

(require 'compat nil t)

(defvar-local backup-real-file nil)

(defun backup--get-list-buffer-for-file (filename)
  (get-buffer-create (format "*backups : %s*" filename)))

(defun backup--get-backup-file-at-point ()
  (or (get-text-property (point) 'backup-file-name (current-buffer))
      (user-error "Not backup file at point")))

(defun backup-jump (backup-file)
  "Jump to BACKUP-FILE."
  (interactive (list (backup--get-backup-file-at-point)) backup-list-mode)
  (find-file-read-only backup-file))

(defun backup-diff (backup-file)
  "Diff BACKUP-FILE with the real file."
  (interactive (list (backup--get-backup-file-at-point)) backup-list-mode)
  (let ((backup-buffer (find-file-noselect backup-file))
	(real-buffer (find-file-noselect backup-real-file)))
    (diff-buffers backup-buffer real-buffer)))

(defun backup-ediff (backup-file)
  "Run `ediff' on BACKUP-FILE and the real file."
  (interactive (list (backup--get-backup-file-at-point)) backup-list-mode)
  (let ((backup-buffer (find-file-noselect backup-file))
	(real-buffer (find-file-noselect backup-real-file)))
    (ediff-buffers backup-buffer real-buffer)))

(defun backup-restore (backup-file)
  "Restore the file with BACKUP-FILE."
  (interactive (list (backup--get-backup-file-at-point)) backup-list-mode)
  (save-window-excursion
    (backup-diff backup-file)
    (when (y-or-n-p (format "Restore file \"%s\" with \"%s\"? "
			    backup-real-file backup-file))
      (copy-file backup-file backup-real-file t)
      (revert-buffer t t))))

(defun backup-delete (backup-file &optional noconfirm)
  "Delete BACKUP-FILE.
If NOCONFIRM is non-nil, do not ask for confirmation."
  (interactive (list (backup--get-backup-file-at-point)
		     current-prefix-arg)
	       backup-list-mode)
  (save-window-excursion
    (backup-diff backup-file)
    (when (or noconfirm
	      (y-or-n-p (format "Delete backup file \"%s\"? "
				backup-file)))
      (delete-file backup-file)
      (revert-buffer t t))))

(defun backup-list--revert-buffer (&rest _)
  (let ((inhibit-read-only t)
	(saved-point (line-beginning-position)))
    (erase-buffer)
    (let ((backup-files (file-backup-file-names backup-real-file)))
      (if backup-files
	  (insert "Backup files for " backup-real-file ?\n ?\n)
	(insert "No backup files for " backup-real-file ?\n))
      (dolist (backup-file backup-files)
	(backup--insert-entry backup-real-file backup-file)))
    (goto-char saved-point)
    nil))

(defun backup-list--bookmark-handler (bookmark)
  (let ((file (alist-get 'real-file bookmark)))
    (set-buffer (backup-list-backups file))))

(declare-function bookmark-make-record-default "bookmark")

(defun backup-list--bookmark-make-record ()
  `(,@(bookmark-make-record-default 'no-file 'no-context)
    (real-file . ,backup-real-file)
    (handler . backup-list--bookmark-handler)))

(defvar-keymap backup-list-mode-map
  "RET" #'backup-jump
  "=" #'backup-diff
  "e" #'backup-ediff
  "r" #'backup-restore
  "d" #'backup-delete
  "p" #'previous-line
  "n" #'next-line)

;;;###autoload
(define-derived-mode backup-list-mode special-mode "Backup list mode"
  "Major mode for displaying list of backup files."
  :group 'backup
  (setq-local revert-buffer-function #'backup-list--revert-buffer)
  (setq-local bookmark-make-record-function #'backup-list--bookmark-make-record))

(defun backup-list-default-format-entry-function (file backup-file)
  (let* ((basename (file-name-nondirectory file))
	 (version (progn
		    (string-match file-name-version-regexp backup-file)
		    (match-string 0 backup-file)))
	 (name (concat basename version))
	 (attrs (file-attributes backup-file))
	 (time (file-attribute-modification-time attrs))
	 (uid (file-attribute-user-id attrs))
	 (gid (file-attribute-group-id attrs))
	 (size (file-attribute-size attrs)))
    (with-temp-buffer
      (indent-to 2)
      (insert name)
      (indent-to 40)
      (insert (format-time-string "%F %T" time))
      (when (or (/= (user-uid) uid) (/= (group-gid) gid))
	(insert ?\s
		(or (user-login-name uid) uid)
		?:
		(or (group-name gid) gid)))
      (indent-to 64)
      (insert (file-size-human-readable size 'iec " "))
      (add-text-properties (line-beginning-position) (line-end-position)
			   `(backup-file-name ,backup-file)
			   (current-buffer))
      (newline)
      (buffer-string))))

(defcustom backup-list-format-entry-function #'backup-list-default-format-entry-function
  "Function used to format backup list entries."
  :type 'function
  :group 'backup)

(defun backup--insert-entry (file backup-file)
  (insert (funcall backup-list-format-entry-function file backup-file)))

;;;###autoload
(defun backup-list-backups (file)
  "List backups of FILE."
  (interactive
   (list (if (or current-prefix-arg (not (buffer-file-name)))
	     (read-file-name "List backups of file: ")
	   (buffer-file-name))))
  (with-current-buffer (backup--get-list-buffer-for-file file)
    (backup-list-mode)
    (setq backup-real-file file)
    (backup-list--revert-buffer)
    (pop-to-buffer (current-buffer))
    (current-buffer)))

;;;###autoload
(defalias 'list-backups #'backup-list-backups)

(provide 'backup)
;;; backup.el ends here
