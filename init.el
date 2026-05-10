;;; init.el --- Emacs configuration file           -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: local

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

;; This is my personal Emacs configuration file.

;;; Code:

(setq debug-on-error init-file-debug)

(require 'early-init early-init-file t)
(when (featurep 'init)
  (load early-init-file nil t))

;;;; pre-init.el

(defvar pre-init-file (locate-user-emacs-file "pre-init.el")
  "The file to load before the init file.")

(when (file-exists-p pre-init-file)
  (load pre-init-file nil t))


(require 'dotemacs-ui)
(require 'dotemacs-editor)
(require 'dotemacs-llm)
(require 'dotemacs-bindings)
(require 'dotemacs-lsp)
(require 'dotemacs-completion)
(require 'dotemacs-tex)
(require 'dotemacs-tramp)
(require 'dotemacs-org)
(require 'dotemacs-eshell)
(require 'dotemacs-dired)
(require 'dotemacs-vc)
(require 'dotemacs-nix)
(require 'dotemacs-lisp)
(require 'dotemacs-misc)
(require 'dotemacs-prog)
(require 'dotemacs-help)
(require 'dotemacs-email)
(require 'dotemacs-text)
(require 'dotemacs-modal)
(require 'dotemacs-theme)
(require 'dotemacs-denote)
(require 'dotemacs-chinese)
(require 'dotemacs-treesit)
(require 'dotemacs-security)

;;;; custom

(defvar custom-file)
(and custom-file
     (load custom-file t))


;;;; post-init

(defvar post-init-file (locate-user-emacs-file "post-init.el"))

(when (file-exists-p post-init-file)
  (load post-init-file nil t))

;;;; _

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (aggressive-indent-mode)
;; End:

;;; init.el ends here
