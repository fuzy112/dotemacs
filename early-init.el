;;; early-init.el --- Early init file                -*- lexical-binding: t; -*-

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

;;

;;; Code:

;;;; gc

(when (fboundp 'igc-start-idle-timer)
  (add-hook 'emacs-startup-hook #'igc-start-idle-timer))

(defvar gc-threshold-initialized nil
  "Non-nil when `init-gc-threshold' has been executed.")

(defun init-gc-threshold ()
  "Restore `gc-cons-threshold' to its default value.
This function is intended to be called once after Emacs startup
to revert the temporary large value set during initialization."
  ;; Reset gc-cons-threshold to the default compiled-in value.
  (set-default-toplevel-value
   'gc-cons-threshold
   (eval (car (get 'gc-cons-threshold 'standard-value))))
  ;; Mark as initialized so the guard below won't reapply the temporary value.
  (setq gc-threshold-initialized t))

;; During startup, set gc-cons-threshold to a very large value to
;; avoid excessive garbage collection.  This is a common optimisation.
(unless gc-threshold-initialized
  (setq gc-cons-threshold most-positive-fixnum)
  ;; Schedule restoration of the default threshold after init.
  (if noninteractive
      ;; In batch mode, emacs-startup-hook is not run.
      (add-hook 'after-init-hook 'init-gc-threshold)
    (add-hook 'emacs-startup-hook 'init-gc-threshold)))

;;;; File loading

(setq load-prefer-newer t)
(setq load-path-filter-function #'load-path-filter-cache-directory-files)

;;;; pre-early-init

(defvar pre-early-init-file (locate-user-emacs-file "pre-early-init.el")
  "The file to load before `early-init'.")

(when (file-exists-p pre-early-init-file)
  (load pre-early-init-file nil t))



;;;; emacs core

(setq current-time-list nil)

(setq-default native-comp-async-on-battery-power nil)

(setq use-file-dialog    nil
      use-dialog-box     nil
      use-short-answers  t)
(setq system-time-locale "C")

(setq undo-limit        (* 16 1024 1024)
      undo-strong-limit (* 64 1024 1024))

;; ;; workaround WSL wayland clipboard issue
;; (setq select-active-regions nil)

(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


(setq inhibit-default-init  t
      inhibit-splash-screen t)

(setq default-frame-alist `((vertical-scroll-bars   . nil)
                            (horizontal-scroll-bars . nil)))

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(setq window-combination-resize t)

(setq set-mark-command-repeat-pop t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq redisplay-skip-fontification-on-input t)

(setq read-process-output-max 1048576)

;;;; jkr

(setq jka-compr-load-suffixes (seq-union '(".zst") jka-compr-load-suffixes))
(setq jka-compr-verbose nil)

;;;; files

(setq version-control     t
      delete-old-versions t
      kept-old-versions   9
      kept-new-versions   9
      require-final-newline 'ask)

;;;; custom

(setq custom-file (locate-user-emacs-file "var/custom.el.zst"))


;;;; post-early-init

(defvar post-early-init-file (locate-user-emacs-file "post-early-init.el")
  "The file to load after `early-init'.")

(when (file-exists-p post-early-init-file)
  (load post-early-init-file nil t))


(provide 'early-init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; early-init.el ends here
