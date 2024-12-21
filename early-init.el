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

;;;; gc

(setq gc-cons-threshold 25600000)
(setq read-process-output-max (* 256 1024))

;;;; pre-early-init

(defvar pre-early-init-file (locate-user-emacs-file "pre-early-init.el")
  "The file to load before `early-init'.")

(when (file-exists-p pre-early-init-file)
  (load pre-early-init-file nil t))


(setq package-enable-at-startup nil)
(setq straight-use-version-specific-build-dir t)
(setq straight-enable-use-package-integration t)
(defvar straight-current-profile)
(setq straight-current-profile nil)
(setq straight-profiles '((nil . "default.el")
                          (dotemacs . "dotemacs.el")
                          (user . "user.el")
                          (custom . "custom.el")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7)
      (straight-current-profile nil))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-default-files-directive
      (seq-union straight-default-files-directive
                 '("docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo")))

(let ((straight-current-profile 'dotemacs))
  (load (locate-user-emacs-file "packages.el") nil t))

;;;; site lisp

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/tui"))

(eval-when-compile (require 'dotemacs-core))

;;;; emacs core

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq inhibit-compacting-font-caches t)
(setq use-file-dialog nil)
(setq system-time-locale "C")
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; workaround WSL wayland clipboard issue
(setq select-active-regions nil)

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)

(setq inhibit-default-init t
      inhibit-splash-screen t)

(setq default-frame-alist `((vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (font . "Iosevka SS04-14")
                            (width . 157)
                            (height . 38)
                            (alpha-background . 80)))

;;;; warnings

(setopt warning-suppress-log-types '((bytecomp)))

;;;; files

(setq confirm-kill-emacs 'y-or-n-p)
(setq version-control t
      delete-old-versions t
      kept-old-versions 9
      kept-new-versions 9)

;;;; modus-theme

(after-load! modus-themes
  (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (setq modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-slanted-constructs t
        modus-themes-variable-pitch-ui t))

;;;; doom-modeline

(require 'doom-modeline)
(doom-modeline-mode)

;;;; libraries
(after-load! (:or dash elisp-mode)
  (global-dash-fontify-mode))
(after-load! info-look
  (dash-register-info-lookup))
(after-load! (:and anaphora elisp-mode)
  (anaphora-install-font-lock-keywords))

;;;; custom

(defun +custom-faces (&optional theme)
  (unless (eq theme 'user)
    (defvar pp-posframe-parameters)
    (setq pp-posframe-parameters `( :border-color "gray"
                                    :border-width 1
                                    :background-color ,(face-background 'default nil '(shadow))))
    (custom-set-faces
     '(fill-column-indicator
       ((((type w32 tty))
         :height 1.0 :foreground "gray50" :background unspecified)))
     '(parenthesis
       ((t :inherit shadow)))
     '(header-line
       ((((supports :underline t) (class color grayscale) (background dark))
         :background "black" :underline (:color "white" :style line :position t)
         :box (:line-width 6 :color "black" :style nil))
        (((supports :underline t) (class color grayscale) (background light))
         :background "white" :underline (:color "black" :style line :position t)
         :box (:line-width 6 :color "white" :style nil))))
     '(mode-line-active
       ((((supports :overline t) (class color grayscale) (background dark))
         :background "black" :overline "white"
         :box (:line-width 6 :color "black" :style nil))
        (((supports :overline t) (class color grayscale) (background light))
         :background "white" :overline "black"
         :box (:line-width 6 :color "white" :style nil))))
     '(mode-line-inactive
       ((((supports :overline t) (class color grayscale) (background dark))
         :background "black" :overline "gray70"
         :box (:line-width 6 :color "black" :style nil))
        (((supports :overline t)(class color grayscale) (background light))
         :background "white" :overline "gray30"
         :box (:line-width 6 :color "white" :style nil)))))))
(add-hook 'enable-theme-functions #'+custom-faces t)
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (let ((straight-current-profile 'custom))
    (load custom-file nil t)))
(eval-after-load 'init #'+custom-faces)

(declare-function modus-themes-load-theme "modus-themes.el" (arg1))
(defvar modus-themes-to-toggle)
(unless custom-enabled-themes
  (require 'modus-themes)
  (modus-themes-load-theme (car modus-themes-to-toggle)))


;;;; post-early-init

(defvar post-early-init-file (locate-user-emacs-file "post-early-init.el")
  "The file to load after `early-init'.")

(when (file-exists-p post-early-init-file)
  (let ((straight-current-profile 'user))
    (load post-early-init-file nil t)))


(provide 'early-init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; early-init.el ends here
