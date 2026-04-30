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
;;
;; Key features of this configuration include:
;;
;; - Meow modal editing with a Dvorak layout.
;; - Extensive use of `orderless', `vertico', `consult', `embark',
;;   `corfu', and `marginalia' for a modern completion and navigation
;;   experience.
;; - Integration with `eglot' for LSP support, with automatic discovery
;;   of language servers via Nix.
;; - A custom theme setup based on `modus-themes' with a dedicated
;;   `dotemacs' theme for face customizations.
;; - Font configuration for Iosevka, Sarasa Gothic, and Nerd Fonts.
;; - Support for various programming languages, including C/C++, Python,
;;   Rust, Nix, JavaScript, and more.
;; - Org-mode setup with `denote' for note-taking and knowledge
;;   management.
;; - Magit and version control integration with `diff-hl'.
;; - Terminal emulation via `eat' and `eshell'.
;; - Custom keybindings and utility functions for window management,
;;   side windows, and more.
;;
;; The configuration is split into several sections, each prefixed with
;; a `;;;;' header for easy navigation.

;;; Code:

(require 'early-init early-init-file t)
(when (featurep 'init)
  (load early-init-file nil t))
(require 'dotemacs-core)
(eval-when-compile
  (require 'compat)
  (require 'cond-let))

;;;; pre-init.el

(defvar pre-init-file (locate-user-emacs-file "pre-init.el")
  "The file to load before the init file.")

(when (file-exists-p pre-init-file)
  (load pre-init-file nil t))

;;;; custom

(defvar custom-file)
(and custom-file
     (file-exists-p custom-file)
     (load custom-file))

(require 'dotemacs-ui)
(require 'dotemacs-editor)
(after-load! gptel
  (require 'dotemacs-llm))
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
