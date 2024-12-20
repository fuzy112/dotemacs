;;; pp-posframe.el --- Show pp results in posframe    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: lisp

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

;; This package provides commands to eval and macroexpand expressions
;; and display the results in a posframe.

;;; Code:

(require 'posframe)
(require 'pp)

(defgroup pp-posframe ()
  "Pp Posframe."
  :group 'pp
  :prefix "pp-posframe-")

(defcustom pp-posframe-buffer-name "*Pp Eval Output*"
  "Name of buffer to use for displaying evaluation results."
  :type 'string)

(defcustom pp-posframe-hide-ignore-commands '()
  "A list of commands to ignore when hiding the posframe."
  :type '(repeat symbol))

(defcustom pp-posframe-parameters ()
  "A plist of parameters used to show the posframe."
  :type '(plist :key-type symbol))

(defun pp-posframe--pre-command ()
  "Hide the posframe if `this-command' is not in `pp-posframe--pre-command'."
  (unless (memq this-command pp-posframe-hide-ignore-commands)
    (pp-posframe-mode -1)))

(add-hook 'pre-command-hook #'pp-posframe--pre-command)

(defun pp-posframe-save ()
  "Save the evaluation result into the kill ring."
  (interactive)
  (with-current-buffer pp-posframe-buffer-name
    (set-text-properties (point-min) (point-max) nil)
    (copy-region-as-kill (point-min) (point-max))))

(defun pp-posframe-yank ()
  "Yank the evaluation result into the current buffer."
  (interactive)
  (insert-buffer-substring pp-posframe-buffer-name))

(defun pp-posframe-buffer ()
  "Pop to the evaluation result buffer."
  (interactive)
  (pop-to-buffer pp-posframe-buffer-name))

(defvar-keymap pp-posframe-map
  :doc "The transient keymap to use when displaying the posframe."
  "M-w" #'pp-posframe-save
  "C-y" #'pp-posframe-yank
  "M-o" #'pp-posframe-buffer
  "q" #'ignore)

(define-minor-mode pp-posframe-mode
  "Temporary minor mode for manipulating pp-posframe."
  :keymap pp-posframe-map
  :global t
  (if pp-posframe-mode
      (apply #'posframe-show pp-posframe-buffer-name
	     :min-width 5 pp-posframe-parameters)
    (posframe-hide pp-posframe-buffer-name)))

(defun pp-posframe-display-value (value lexical)
  "Display VALUE in a posframe."
  (with-current-buffer (get-buffer-create pp-posframe-buffer-name)
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (delay-mode-hooks
	(unless (derived-mode-p 'emacs-lisp-mode)
	  (emacs-lisp-mode))
	(defvar pp-default-function)
	(let ((pp-default-function 'pp-fill))
	  (pp value))
	(setq lexical-binding lexical)
	(font-lock-ensure)))
    (add-text-properties 1 2 `(display ,(concat "=> " (buffer-substring 1 2)))))
  (when (posframe-workable-p)
    (pp-posframe-mode))
  (message "=> %S" value)
  (redisplay))

;;;###autoload
(defun pp-posframe-eval-last-sexp ()
  "Evaluate sexp before point; display the value in a posframe."
  (interactive)
  (redisplay)
  (let ((value (eval (macroexpand-all
		      (eval-sexp-add-defvars
		       (elisp--eval-defun-1 (macroexpand (pp-last-sexp)))))
		     lexical-binding)))
    (pp-posframe-display-value value lexical-binding)))

;;;###autoload
(defun pp-posframe-macroexpand-last-sexp ()
  "Macroexpand the sexp before point; display the result in a posframe."
  (interactive)
  (pp-posframe-display-value
   (macroexpand-1 (pp-last-sexp))
   lexical-binding))

;;;###autoload
(defun pp-posframe-compile-defun ()
  "Compile and evaluate the current top-level form.
Display the result in a posframe."
  (interactive)
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (let* ((byte-compile-current-file (current-buffer) )
	   (byte-compile-current-buffer (current-buffer))
	   (start-read-position (point))
	   (byte-compile-last-warned-form 'nothing)
	   (symbols-with-pos-enabled t)
	   (value (eval
		   (displaying-byte-compile-warnings
		    (byte-compile-sexp
		     (let ((form (read-positioning-symbols (current-buffer))))
		       (push form byte-compile-form-stack)
		       (eval-sexp-add-defvars
			form
			start-read-position))))
		   lexical-binding)))
      (end-of-defun)
      (pp-posframe-display-value value lexical-binding))))

(provide 'pp-posframe)
;;; pp-posframe.el ends here
