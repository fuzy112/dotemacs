;;; project-compile-history.el --- Project-specific compile history -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

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

(defcustom pch:length nil
  "Maximum length of history lists of each project.
When nil, use `history-length', which see."
  :type '(choice (const :tag "Default value" nil)
		 natnum)
  :set (lambda (symbol value)
	 (set symbol value)
	 (when (boundp 'pch:obarray)
	   (obarray-map (lambda (var)
			  (put var 'history-length value))
			pch:obarray))
	 value))

(defvar pch:obarray (obarray-make 16)
  "Obarray for project compile history variables.")

(defvar pch:modified nil)

(defun pch:variable (project)
  "Return the per-project history variable symbol for PROJECT.
The variable name is derived from the abbreviated file name of PROJECT's
root directory, interned in the obarray `project-compile-history-obarray'.

If the returned variable is unbound when this function is called,
initialize it to nil.  When `project-compile-history-length' is non-nil,
also set the variable's `history-length' property to the value of
`project-compile-history-length' during initialization."
  (let* ((root (abbreviate-file-name (project-root project)))
	 (hist-var (intern root pch:obarray)))
    (unless (boundp hist-var)
      (set hist-var nil)
      (when pch:length
	(put hist-var 'history-length pch:length)))
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
  (when (or pch:modified current-prefix-arg)
    (let ((pch:-saving t))
      (with-work-buffer
	(insert ";; project-compile-history-file -*- coding: utf-8-emacs; -*-\n\n")
	(insert "(\n")
	(let ((pp-max-width 200)
	      (cell (cons nil nil)))
	  (obarray-map
	   (lambda (var)
	     (setcar cell (symbol-name var))
	     (setcdr cell (symbol-value var))
	     (pp cell (current-buffer)))
	   pch:obarray))
	(insert ")\n")
	(write-region nil nil pch:file))
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
	(let ((hist-var (intern name pch:obarray)))
	  (set hist-var hist)
	  (when pch:length
	    (put hist-var 'history-length pch:length))))))
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
