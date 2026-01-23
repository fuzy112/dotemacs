;; -*- lexical-binding: t; -*-

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

(require 'consult)
(require 'nixos-options)

(defun consult--nixos-option-annotate (candidate)
  "Annotate CANDIDATE NixOS option with type and description.
Return a formatted string showing the option type and first line of description."
  (let* ((option (nixos-options-get-option-by-name candidate))
	 (type (nixos-options-get-type option))
	 (default (nixos-options-get-default option))
	 (description (nixos-options-get-description option))
	 (short-description (with-temp-buffer
			      (insert description)
			      (goto-char (point-min))
			      (buffer-substring (point-min) (line-end-position))))
	 (space-width (max 2
			   (- 16 (length type)))))
    (consult--annotate-align
     candidate
     (concat
      (propertize type 'face 'font-lock-type-face)
      (propertize " " 'display `(space :width ,space-width))
      (propertize short-description 'face font-lock-doc-face)
      ))))

(defun consult--nixos-option-preview ()
  (let ((preview (consult--buffer-preview))
	(orig (buffer-list))
	buffers)
    (lambda (action cand)
      (unless cand
	(pcase-dolist (`(,_ . ,buf) buffers)
	  (kill-buffer buf))
	(setq buffers nil))
      (let ((consult--buffer-display #'switch-to-buffer-other-window))
	(funcall preview action
		 (and cand
		      (eq action 'preview)
		      (or (cdr (assoc cand buffers))
			  (when-let* ((buf (consult--nixos-option-action cand t)))
			    (unless (memq buf orig)
			      (cl-callf consult--preview-add-buffer
				  buffers (cons cand buf)))
			    buf))))))))

(defvar consult--nixos-option-display #'switch-to-buffer-other-window)

(defun consult--nixos-option-action (option-name &optional nodisplay)
  "Display documentation for OPTION-NAME.
When NODISPLAY is non-nil, return the buffer without displaying it.
Return the documentation buffer or nil if option not found."
  (when-let* ((option (nixos-options-get-option-by-name option-name))
	      (doc (nixos-options-get-documentation-for-option option))
	      (buf (nixos-options-doc-buffer doc)))
    (unless nodisplay
      (funcall consult--nixos-option-display buf))
    buf))

(consult--define-state nixos-option)

;;;###autoload
(defun consult-nixos-option (&optional initial)
  "Browse and select NixOS options using consult.
Display a list of all NixOS options with type annotations.
Optionally start with INITIAL input.
Selecting an option displays its documentation."
  (interactive)
  (consult--read
   nixos-options
   :prompt "Options: "
   :initial initial
   :category 'consult-nixos-option
   :annotate #'consult--nixos-option-annotate
   :state (consult--nixos-option-state)))

(provide 'consult-nixos)
;;; consult-nixos.el ends here
