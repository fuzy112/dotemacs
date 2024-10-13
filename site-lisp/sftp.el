;;; sftp.el --- SFTP Integration -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version:  0.1.8
;; Keywords: network comm

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

;; This package provides SFTP integration in Emacs.  Users can use the
;; `sftp' command to connect to a host.

;;; Code:

;;;; Requirements

(require 'net-utils)
(require 'comint)
(require 'auth-source)

;;;; Customization options

(defgroup sftp nil
  "SFTP."
  :group 'net-utils
  :prefix "sftp-")

(defcustom sftp-program "sftp"
  "SFTP program to use."
  :type 'string)

(defcustom sftp-program-options nil
  "Options of sftp."
  :type '(repeat string))

(defcustom sftp-prompt-regexp "^sftp>"
  "Regexp which matches the FTP program's prompt."
  :type 'string)

(defcustom sftp-password-prompt-regexp
  (concat "^\\(?:\\(?1:[^@\n\r]+\\)@\\(?2:[-a-zA-Z0-9.]+\\)'s Password"
	  "\\|(\\(?1:[^@\n\r]+\\)@\\(?2:[-a-zA-Z0-9.]+\\)) Password"
	  "\\): *\\'")
  "Regexp to match password prompts."
  :type 'string)

(defvar sftp-history nil
  "Variable to store history of sftp command.")

(defvar-local sftp--first-password-request t)

(defvar-local sftp--password-save-function nil)

(defvar auth-source-creation-prompts)

;;;; Internal functions

(defun sftp--password-function (prompt)
  (or
   (and (string-match sftp-password-prompt-regexp prompt)
	sftp--first-password-request
	(let* ((case-fold-search t)
	       (user (match-string 1 prompt))
	       (host (match-string 2 prompt))
	       (auth-source-creation-prompts `((secret . ,prompt)))
	       (auth-info
		(car
		 (auth-source-search
		  :max 1 :user user :host host :port "sftp"
		  :require '(:secret)
		  :create t)))
	       (auth-pass (auth-info-password auth-info)))
	  (setq sftp--password-save-function (plist-get auth-info :save-function))
	  (setq sftp--first-password-request nil)
	  auth-pass))
   (read-passwd prompt)))

(defun sftp--watch-for-connected (string)
  (and (string-match-p (concat "Connected to .*") string)
       (message "Saving password")
       (functionp sftp--password-save-function)
       (funcall sftp--password-save-function)
       (setq sftp--password-save-function nil)))

(defun sftp--candidates (proc input)
  (let (throw-on-input
	prompt-begin kept completions new-input)
    (unwind-protect
	(progn
          (setq kept "")
	  (set-process-filter proc (lambda (_p s) (setq kept (concat kept s))))
	  (process-send-string proc (concat input "\t"))
	  ;; Wait for the prompt to appear
	  (while (and (not (setq prompt-begin (string-match sftp-prompt-regexp kept)))
		      (accept-process-output proc 2 nil t)))
	  (if (not prompt-begin)
	      ;; no completions
	      nil
            (setq new-input (substring kept (1+ (match-end 0))))
	    (if (not (string= input new-input))
		;; sole completion
		(list new-input)
	      ;; remove the input and prompt from the kept string
	      (setq kept (substring kept (length input) prompt-begin))
	      ;; split the completions
	      (setq completions (split-string kept))
	      ;; (message "completions: %S" completions)
              (string-match "\\(.*?\\)[^/ ]*$" input) ;TODO handle escaped characters
	      (let ((prefix (match-string 1 input)))
		(setq completions (mapcar (lambda (c) (concat prefix c)) completions)))
	      completions)))
      (process-send-string proc "")
      (while (accept-process-output proc 0.1 nil t))
      (set-process-filter proc #'comint-output-filter))))

(defun sftp--completion-table (beg end)
  (let ((beg (copy-marker beg))
	(end (copy-marker end))
	(input 'init)
	(buf (current-buffer))
	completions )
    (lambda (str pred action)
      (unless (or (eq action 'metadata) (eq (car-safe action) 'boundaries))
	(let ((new-input (with-current-buffer buf
			   (buffer-substring-no-properties beg end))))
	  (when (or (eq input 'init)
		    (not (string-prefix-p input new-input))
		    (string-suffix-p "/" new-input))
	    (setq input new-input)
	    (setq completions (sftp--candidates (get-buffer-process buf) input)))
	  (complete-with-action action completions str pred))))))

(defun sftp--capf ()
  (let ((beg (save-excursion (move-beginning-of-line 1) (point)))
	(end (point)))
    (list beg end (sftp--completion-table beg end)
	  :exclusive 'no :company-docsig #'ignore)))

;;;; Commands

;;;###autoload
(defun sftp (host)
  "Run `sftp-program' to connect to HOST."
  (interactive
   (list (let ((default (ffap-machine-at-point)))
           (read-string (format-prompt "Sftp to Host" default)
                        nil 'sftp-history default))))
  (let ((buf (get-buffer-create (concat "*sftp [" host "]*"))))
    (set-buffer buf)
    (let ((ftp-prompt-regexp sftp-prompt-regexp))
      (ftp-mode))
    (setq-local coding-system-for-read 'utf-8-dos)
    (setq-local comint-process-echoes t)
    (setq-local comint-password-prompt-regexp sftp-password-prompt-regexp)
    (setq-local comint-password-function #'sftp--password-function)
    (setq-local sftp--first-password-request t)
    (add-hook 'comint-output-filter-functions #'sftp--watch-for-connected
	      nil t)
    (add-hook 'comint-dynamic-complete-functions #'sftp--capf nil t)
    (comint-exec buf (concat "sftp-" host) sftp-program nil
                 (append (list host) sftp-program-options))
    (pop-to-buffer buf)))

(provide 'sftp)
;;; sftp.el ends here
