;;; early-init.el --- Early init file                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

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

;;

;;; Code:

;;;; pre-early-init

(defvar pre-early-init-file (locate-user-emacs-file "pre-early-init.el")
  "The file to load before `early-init'.")

(when (file-exists-p pre-early-init-file)
  (load pre-early-init-file nil t))



;;;; use-package

(setq use-package-enable-imenu-support t)
(eval-when-compile (require 'use-package))

;;;; emacs core

(use-package emacs
  :config
  (setq gc-cons-threshold 25600000)
  (setq read-process-output-max (* 256 1024))
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setq inhibit-compacting-font-caches t)
  (setq use-file-dialog nil)
  (setq system-time-locale "C")
  (setq bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t)

  (setq menu-bar-mode nil)
  (setq tool-bar-mode nil)

  (setq inhibit-default-init t
        inhibit-splash-screen t)

  (setq default-frame-alist `((vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              (font . "Iosevka SS04-14")
                              (width . 174)
                              (height . 46)
                              (alpha-background . 80)
                              (alpha . 80)))

  (set-fontset-font t 'han "Sarasa Gothic CL"))

;;;; package

(use-package package
  :defer
  :init
  (package-activate-all)
  :config
  (setq package-install-upgrade-built-in t)
  (package-initialize)
  (add-hook 'kill-emacs-hook #'package-quickstart-refresh -50))

;;;; Site lisp

(use-package site-lisp
  :load-path "site-lisp"
  :config
  (site-lisp-activate))


(defun find-early-init-file ()
  "Find `early-init-file'."
  (interactive)
  (find-file early-init-file))

(defun find-user-init-file ()
  "Find `user-init-file'."
  (interactive)
  (find-file user-init-file))

(use-package files
  :config
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq version-control t
        delete-old-versions t
        kept-old-versions 9
        kept-new-versions 9))

;;;; modus-theme

(use-package modus-themes
  :defer t
  :bind
  ("<f9>" . modus-themes-toggle)
  :config
  (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (setq modus-themes-custom-auto-reload nil
        modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui nil))

;;;; faces

(use-package faces
  :config
  (cl-pushnew "Sarasa Fixed CL" (alist-get "Monospace" face-font-family-alternatives nil nil #'string=))
  (cl-pushnew "Sarasa Fixed Slab CL" (alist-get "Monospace Serif" face-font-family-alternatives nil nil #'string=))
  (cl-pushnew "Sarasa Gothic CL" (alist-get "Sans Serif" face-font-family-alternatives nil nil #'string=)))

;;;; custom

(use-package custom
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  :config
  (load custom-file nil t)
  (unless custom-enabled-themes
    (require 'modus-themes)
    (modus-themes-load-theme 'modus-vivendi)))


;;;; post-early-init

(defvar post-early-init-file (locate-user-emacs-file "post-early-init.el")
  "The file to load after `early-init'.")

(when (file-exists-p post-early-init-file)
  (load post-early-init-file nil t))


(provide 'early-init)

;; Local Variables:
;; eval: (outline-minor-mode)
;; indent-tabs-mode: nil
;; End:

;;; early-init.el ends here
