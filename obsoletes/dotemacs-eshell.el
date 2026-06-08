;;; dotemacs-eshell.el  -*- lexical-binding: t; -*-

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

;;;; eshell

(defun +eshell/other-window ()
  "Open an EShell buffer in other window."
  (interactive)
  (with-suppressed-warnings ((obsolete display-comint-buffer-action))
    (defvar display-comint-buffer-action)
    (let ((display-comint-buffer-action '(() (inhibit-same-window . t)))
          (display-buffer-alist (cons '((category . comint)
                                        nil
                                        (inhibit-same-window . t))
                                      display-buffer-alist)))
      (eshell))))

(defvar eshell-hist-mode-map)
(after-load! eshell
  (setopt eshell-scroll-to-bottom-on-input 'this
          eshell-history-size 8192
          eshell-save-history-on-exit t)
  (setopt eshell-modules-list (seq-union '(eshell-tramp) (symbol-value 'eshell-modules-list)))
  (defun +eshell--capf ()
    (when (fboundp 'cape-history)
      (add-hook 'completion-at-point-functions #'cape-history 50 t)))
  (add-hook 'eshell-mode-hook '+eshell--capf)
  (after-load! esh-hist
    (when (fboundp 'consult-history)
      (keymap-set eshell-hist-mode-map "M-r" #'consult-history)))
  (after-load! em-term
    (cl-pushnew "journalctl" eshell-visual-commands :test #'equal)
    (alist-setq! eshell-visual-subcommands
      "git" '("log" "diff" "show" "grep")
      "systemctl" '("status" "cat" "show")))
  (require 'eshell-extras))

(provide 'dotemacs-eshell)
;;; dotemacs-eshell.el ends here
