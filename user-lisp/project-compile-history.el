;;; project-compile-history.el --- Project-specific compile history -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "30.1"))
;; Version: 0.1.0
;; Keywords: processes

;;; Commentary:
;;; Code:

;;;;

(require 'project)

(defgroup project-compile-history ()
  "Project-specific compile history"
  :group 'project
  :prefix "project-compile-history-")

(defcustom project-compile-history-file
  (expand-file-name "project-compile-history.eld" user-emacs-directory)
  "File to save project compilation histroy."
  :type 'file)

(defvar project-compile-history-obarray (obarray-make 16)
  "Obarray for project compile history variables.")

(defvar project-compile-history-modified nil)

(defun project-compile-history-variable (project)
  (let* ((root (abbreviate-file-name (project-root project)))
	 (hist-var (intern root project-compile-history-obarray)))
    (unless (boundp hist-var)
      (set hist-var nil))
    hist-var))

(defun project-compile-history-read-command (command)
  (let ((hist-var (project-compile-history-variable (project-current))))
    (prog1
	(read-shell-command "Compile command: " command
			    (if (equal (car (symbol-value hist-var)) command)
				(cons hist-var 1)
			      hist-var))
      (setq project-compile-history-modified t))))

(defun project-compile-history-save ()
  (interactive)
  (when project-compile-history-modified
    (with-temp-file project-compile-history-file
      (insert ";; project-compile-history-file -*- coding: utf-8-emacs; -*-\n\n")
      (insert "(\n")
      (obarray-map
       (lambda (var)
	 (insert "(")
	 (prin1 (symbol-name var) (current-buffer))
	 (insert " . ")
	 (prin1 (symbol-value var) (current-buffer))
	 (insert ")\n"))
       project-compile-history-obarray)
      (insert ")\n"))
    (setq project-compile-history-modified nil)))

(defun project-compile-history-load ()
  (interactive)
  (when (file-exists-p project-compile-history-file)
    (with-temp-buffer
      (insert-file-contents project-compile-history-file)
      (goto-char (point-min))
      (let* ((alist (read (current-buffer))))
	(pcase-dolist (`(,name . ,hist) alist)
	  (set (intern name project-compile-history-obarray) hist))))
    (setq project-compile-history-modified nil)))

(declare-function compilation-read-command "compile")

(defun project-compile-history--advice (&rest args)
  (cl-letf*
      ((project (project-current t))
       (hist-var (project-compile-history-variable project))
       (compile-command (or (car-safe (symbol-value hist-var)) compile-command))
       ((symbol-function #'compilation-read-command)
	#'project-compile-history-read-command))
    (apply args)))

;;;###autoload
(define-minor-mode project-compile-history-mode
  "Separate project-compile history for each project."
  :global t
  (advice-remove #'project-compile #'project-compile-history--advice)
  (remove-hook 'kill-emacs-hook #'project-compile-history-save)
  (when project-compile-history-modified
    (project-compile-history-save))
  (when project-compile-history-mode
    (advice-add #'project-compile :around #'project-compile-history--advice)
    (add-hook 'kill-emacs-hook #'project-compile-history-save)
    (project-compile-history-load)))

(provide 'project-compile-history)
;;; project-compile-history.el ends here
