;;; dotemacs-text.el  -*- lexical-binding: t; -*-

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

;;;; text-mode

(defun +text-mode--capf ()
  "Addd `cape-dict' to the buffer local value of `completion-at-point-functions'."
  (when (fboundp 'cape-dict)
    (add-hook 'completion-at-point-functions #'cape-dict 90 t)))
(add-hook 'text-mode-hook #'+text-mode--capf)


;;;; nxml-mode

(defun +nxml-mode--flymake ()
  (when (fboundp 'flymake-xmllint)
    (add-hook 'flymake-diagnostic-functions #'flymake-xmllint nil t)))
(add-hook 'nxml-mode-hook #'+nxml-mode--flymake)

;;;; yaml

(add-to-list 'auto-mode-alist '("/\\.clang\\(?:d\\|-format\\)\\'" . yaml-mode))

;;;; cmake

(add-hook 'cmake-mode-hook #'cmake-capf-setup)
(add-hook 'cmake-ts-mode-hook #'cmake-capf-setup)

;;;; qmake

(alist-setq! auto-mode-alist
  "\\.pr[oi]\\'" 'makefile-mode)

;;;; systemd-mode

(defun +systemd-mode--setup ()
  (flymake-mode 1)
  (when (fboundp 'flymake-systemd)
    (add-hook 'flymake-diagnostic-functions #'flymake-systemd nil t)))

(add-hook 'systemd-mode-hook #'+systemd-mode--setup)

(provide 'dotemacs-text)
;;; dotemacs-text.el ends here
