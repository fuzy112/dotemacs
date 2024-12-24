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

(setq auto-save-no-message t)

;;;; files

(setq confirm-kill-emacs 'y-or-n-p)
(setq version-control t
      delete-old-versions t
      kept-old-versions 9
      kept-new-versions 9)

;;;; custom

(setq custom-file (locate-user-emacs-file "custom.el"))


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
