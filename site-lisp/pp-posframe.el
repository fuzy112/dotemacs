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

;;

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

(defun pp-posframe--pre-command ()
  "Hide the posframe if `this-command' is not in `pp-posframe--pre-command'."
  (unless (memq this-command pp-posframe-hide-ignore-commands)
    (posframe-hide pp-posframe-buffer-name)))

(add-hook 'pre-command-hook #'pp-posframe--pre-command)

(defun pp-posframe-save ()
  "Save the evaluation result into the kill ring."
  (interactive)
  (with-current-buffer pp-posframe-buffer-name
    (kill-region (point-min) (point-max))))

(defun pp-posframe-yank ()
  "Yank the evaluation result into the current buffer."
  (interactive)
  (insert-buffer-substring-no-properties pp-posframe-buffer-name))

(defun pp-posframe-buffer ()
  "Pop to the evaluation result buffer."
  (interactive)
  (pop-to-buffer pp-posframe-buffer-name))

(defvar-keymap pp-posframe-map
  :doc "The transient keymap to use when displaying the posframe."
  "M-w" #'pp-posframe-save
  "C-y" #'pp-posframe-yank
  "b" #'pp-posframe-buffer
  "q" #'ignore)

(defun pp-posframe-display-value (value lexical)
  "Display VALUE in a posframe."
  (with-current-buffer (get-buffer-create "*Pp Eval Output*")
    (erase-buffer)
    (delay-mode-hooks
      (unless (derived-mode-p 'emacs-lisp-mode)
	(emacs-lisp-mode))
      (let ((pp-default-function 'pp-emacs-lisp-code))
	(pp value (current-buffer)))
      (setq lexical-binding lexical)
      (font-lock-ensure))
    (add-text-properties 1 2 `(display ,(concat "=> " (buffer-substring 1 2)))))
  (posframe-show pp-posframe-buffer-name
		 :position (point)
		 :border-width 1)
  (set-transient-map pp-posframe-map))

;;;###autoload
(defun pp-posframe-eval-last-sexp ()
  "Evaluate sexp before point; display the value in a posframe."
  (interactive)
  (let ((value (eval (macroexpand-all
		      (eval-sexp-add-defvars
		       (elisp--eval-defun-1 (macroexpand (elisp--preceding-sexp)))))
		     lexical-binding)))
    (pp-posframe-display-value value lexical-binding)))

(provide 'pp-posframe)
;;; pp-posframe.el ends here
