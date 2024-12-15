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

(setq straight-current-profile 'dotemacs)

;;;; setup

(straight-use-package 'setup)
(require 'setup)

(eval-and-compile
  (setup-define :straight
    (lambda (package)
      `(straight-use-package ',package))
    :documentation "Install PACKAGE with `straight'.
The first PACKAGE can be used to deduce the feature context."
    :repeatable t
    :shorthand (lambda (package) (or (car-safe (cadr package)) (cadr package)))))

;;;; delight
(eval-and-compile
  (setup (:straight delight)
    (setup-define :delight
      (lambda (&optional spec value)
        `(delight ',(or spec (setup-get 'mode)) ,value t))
      :after-loaded t
      :documentation "Hide the mode lighter.")))

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

  ;; workaround WSL wayland clipboard issue
  (setq select-active-regions 'only)

  (setq menu-bar-mode nil)
  (setq tool-bar-mode nil)

  (setq inhibit-default-init t
        inhibit-splash-screen t)

  (setq default-frame-alist `((vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              (font . "Iosevka SS04-14")
                              (width . 157)
                              (height . 38)
                              (alpha-background . 80))))

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

;;;; doom-modeline
(setup (:straight doom-modeline)
  (doom-modeline-mode))

;;;; libraries
(straight-use-package 's)
(straight-use-package 'f)
(setup (:straight dash)
  (:when-loaded
    (global-dash-fontify-mode)
    (eval-after-load 'info-look #'dash-register-info-lookup)))
(straight-use-package 'transducers)
(setup (:straight anaphora)
  (:when-loaded
    (:with-feature lisp-mode
      (:when-loaded
        (anaphorra-install-font-lock-keywords)))))

;;;; site-lisp

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/tui"))

;;;; custom

(setup custom
  (defun +custom-faces (&optional theme)
    (unless (eq theme 'user)
      (setq pp-posframe-parameters `( :border-color ,(face-background 'border nil '(shadow))
                                      :background-color ,(face-background 'default nil '(shadow))))
      (let ((c '((class color) (min-colors 256)))
            (bg-color (frame-parameter nil 'background-color))
            (fg-color (frame-parameter nil 'foreground-color))
            (shadow-color (face-foreground 'shadow)))
        (custom-set-faces
         `(fill-column-indicator ((,c :height 1.0 :foreground ,shadow-color :background unspecified)))
         `(parenthesis ((,c :foreground ,shadow-color)))
         `(mode-line-active ((,c :background ,bg-color :overline ,fg-color
                                 :box (:line-width 6 :color ,bg-color :style nil))))
         `(mode-line-inactive ((,c :background ,bg-color :overline ,shadow-color
                                   :box (:line-width 6 :color ,bg-color :style nil))))))))
  (add-hook 'enable-theme-functions #'+custom-faces t)
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (let ((straight-current-profile 'custom))
      (load custom-file nil t)))
  (unless custom-enabled-themes
    (require 'modus-themes)
    (modus-themes-load-theme (car modus-themes-to-toggle)))
  (eval-after-load 'init #'+custom-faces))


;;;; post-early-init

(defvar post-early-init-file (locate-user-emacs-file "post-early-init.el")
  "The file to load after `early-init'.")

(when (file-exists-p post-early-init-file)
  (let ((straight-current-profile 'user))
    (load post-early-init-file nil t)))


(provide 'early-init)

;; Local Variables:
;; eval: (outline-minor-mode)
;; indent-tabs-mode: nil
;; End:

;;; early-init.el ends here
