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
(require 'filenotify)

(defgroup project-compile-history ()
  "Project-specific compile history"
  :group 'project
  :prefix "project-compile-history-")

(defcustom pch:file
  (expand-file-name "project-compile-history.eld" user-emacs-directory)
  "File to save project compilation history."
  :type 'file)

(defcustom pch:autorevert t
  "Automatically reload the `project-compile-history-file'."
  :type 'boolean)

(defvar pch:obarray (obarray-make 16)
  "Obarray for project compile history variables.")

(defvar pch:modified nil)

(defun pch:variable (project)
  (let* ((root (abbreviate-file-name (project-root project)))
	 (hist-var (intern root pch:obarray)))
    (unless (boundp hist-var)
      (set hist-var nil))
    hist-var))

(defun pch:read-command (command)
  (let ((hist-var (pch:variable (project-current))))
    (prog1
	(read-shell-command "Compile command: " command
			    (if (equal (car (symbol-value hist-var)) command)
				(cons hist-var 1)
			      hist-var))
      (setq pch:modified t)
      (pch:-schedule-save))))

(defvar pch:-saving nil)

(defun pch:save ()
  (interactive)
  (when pch:modified
    (let ((pch:-saving t))
      (with-temp-file pch:file
        (insert ";; project-compile-history-file -*- coding: utf-8-emacs; -*-\n\n")
        (insert "(\n")
        (obarray-map
         (lambda (var)
	   (insert "(")
	   (prin1 (symbol-name var) (current-buffer))
	   (insert " . ")
	   (prin1 (symbol-value var) (current-buffer))
	   (insert ")\n"))
         pch:obarray)
        (insert ")\n"))
      (setq pch:modified nil))))

(defvar pch:-save-timer nil)

(defun pch:-schedule-save ()
  (when (timerp pch:-save-timer)
    (cancel-timer pch:-save-timer))
  (setq pch:-save-timer
	(run-with-idle-timer 1.0 nil #'pch:save)))

(defun pch:load ()
  (interactive)
  (unless (file-exists-p pch:file)
    (let ((pch:modified t))
      (pch:save)))
  (with-temp-buffer
    (insert-file-contents pch:file)
    (goto-char (point-min))
    (let* ((alist (read (current-buffer))))
      (pcase-dolist (`(,name . ,hist) alist)
	(set (intern name pch:obarray) hist))))
  (setq pch:modified nil))

(declare-function compilation-read-command "compile")

(defun pch:-advice (&rest args)
  (cl-letf*
      ((project (project-current t))
       (default-directory (project-root project))
       (hist-var (pch:variable project))
       (compile-command (or (car-safe (symbol-value hist-var)) compile-command))
       ((symbol-function #'compilation-read-command)
	#'pch:read-command))
    (apply args)))

(defvar pch:-watch-descriptor nil)

;;;###autoload
(define-minor-mode pch:mode
  "Separate project-compile history for each project."
  :global t
  (when (file-notify-valid-p pch:-watch-descriptor)
    (file-notify-rm-watch pch:-watch-descriptor))
  (setq pch:-watch-descriptor nil)
  (advice-remove #'project-compile #'pch:-advice)
  (remove-hook 'kill-emacs-hook #'pch:save)
  (when (timerp pch:-save-timer)
    (cancel-timer pch:-save-timer))
  (setq pch:-save-timer nil)
  (when pch:modified
    (pch:save))
  (when pch:mode
    (advice-add #'project-compile :around #'pch:-advice)
    (add-hook 'kill-emacs-hook #'pch:save)
    (pch:load)
    (when pch:autorevert
      (setq pch:-watch-descriptor
	    (file-notify-add-watch
	     pch:file
	     '(change attribute-change)
	     (lambda (event)
	       (let* ((descriptor (car event))
		      (action (nth 1 event))
		      (_file (nth 2 event))
		      (_file1 (nth 3 event)))
		 (cond ((eq action 'stopped)
			(file-notify-rm-watch descriptor)
			(setq pch:-watch-descriptor nil))
		       ((and (memq action '(attribute-changed rename changed created))
			     (not pch:-saving))
			(pch:load))))))))))

(provide 'project-compile-history)
;; Local Variables:
;; read-symbol-shorthands: (("pch:" . "project-compile-history-"))
;; End:
;;; project-compile-history.el ends here
