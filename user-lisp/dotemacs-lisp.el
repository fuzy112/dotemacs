;;; dotemacs-lisp.el  -*- lexical-binding: t; -*-

;; Copyright © 2024-2026  Zhengyi Fu <i@fuzy.me>

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


(eval-when-compile (require 'dotemacs-core))

;;;; paren-face

(add-hook 'lisp-data-mode-hook #'paren-face-mode)
(add-hook 'scheme-mode-hook #'paren-face-mode)

;;;; paredit

(add-hook 'lisp-data-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)

(after-load! paredit
  (define-keymap :keymap paredit-mode-map
    "C-c DEL" #'backward-delete-char
    "C-c <delete>" #'delete-char
    "M-s" nil
    "M-D" #'paredit-splice-sexp
    "M-?" nil
    "C-<right>" nil
    "C-<left>" nil
    "M-<up>" "C-M-<backspace>"
    "M-<down>" "C-M-<delete>"))

;;;; lisp

(after-load! sly
  (keymap-set sly-prefix-map "M-h" #'sly-documentation-lookup))

;;;; elisp-mode

(add-hook 'emacs-lisp-mode-hook #'cursor-sensor-mode)

(setopt elisp-fontify-semantically t)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (locate-user-emacs-file "site-lisp/")))
(when (native-comp-available-p)
  (keymap-set emacs-lisp-mode-map "C-c C-l" #'emacs-lisp-native-compile-and-load))
(keymap-set emacs-lisp-mode-map "C-x C-M-x" #'compile-defun)
(keymap-set lisp-interaction-mode-map "C-c C-j" #'eval-print-last-sexp)

(after-load! find-func
  (setopt find-library-include-other-files nil))

;;;; libraries
(after-load! (:or dash elisp-mode)
  (global-dash-fontify-mode))
(after-load! info-look
  (dash-register-info-lookup))
(after-load! (:and anaphora elisp-mode)
  (anaphora-install-font-lock-keywords))
(after-load! cond-let
  (font-lock-add-keywords 'emacs-lisp-mode
                          cond-let-font-lock-keywords t))


(defun find-user-emacs-file ()
  "Find `early-init-file'."
  (interactive)
  (consult-find user-emacs-directory))

(defun find-user-init-file ()
  "Find `user-init-file'."
  (interactive)
  (find-file user-init-file))


(defvar pp-default-function)

(defun copy-json-as-lisp (beg end)
  "Convert JSON between BEG–END to Lisp data and copy to kill ring.
With no active region, operate on the whole buffer."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((str (buffer-substring-no-properties beg end))
	(pp-default-function 'pp-29))
    (if (string-blank-p str)
	(user-error "Empty string")
      (kill-new
       (pp-to-string
	(json-parse-string str :object-type 'alist))))))


(provide 'dotemacs-lisp)
;;; dotemacs-lisp.el ends here
