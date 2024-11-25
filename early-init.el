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


(setq package-enable-at-startup nil)
(setq straight-use-version-specific-build-dir t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'setup)
(straight-use-package 'delight)
(straight-use-package 's)
(straight-use-package 'f)
(straight-use-package 'dash)

;;;; emacs core

(setup emacs
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
                              (width . 157)
                              (height . 38)
                              (alpha-background . 80)
                              (alpha . 80))))

(setup files
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq version-control t
        delete-old-versions t
        kept-old-versions 9
        kept-new-versions 9))

;;;; modus-theme

(straight-use-package 'modus-themes)

(setup modus-themes
  (:global "<f9>" modus-themes-toggle)
  (:when-loaded
    (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
    (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
    (setq modus-themes-mixed-fonts t
          modus-themes-bold-constructs t
          modus-themes-slanted-constructs t
          modus-themes-variable-pitch-ui t)))

;;;; site-lisp

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/tui"))


;;;; custom

(setup custom
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file nil t))
  (unless custom-enabled-themes
    (require 'modus-themes)
    (modus-themes-load-theme (car modus-themes-to-toggle))))


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
