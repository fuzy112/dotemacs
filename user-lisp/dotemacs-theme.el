;;; dotemacs-theme.el  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'dotemacs-core)
  (require 'llama)
  (require 'modus-themes))

;;;; themes

;; Any Elisp file can run arbitrary code, so there is no reason to
;; handle themes differently.
(setopt custom-safe-themes t)

(defun +inhibit-implied-resize (&rest args)
  "Prevent implied resize when switching themes."
  (let ((frame-inhibit-implied-resize t))
    (apply args)))

(advice-add 'enable-theme :around #'+inhibit-implied-resize)
(advice-add 'disable-theme :around #'+inhibit-implied-resize)

;;;; modus-theme

(after-load! modus-themes
  (setopt modus-themes-mixed-fonts        t
          modus-themes-bold-constructs    t
          modus-themes-slanted-constructs t
          modus-themes-variable-pitch-ui  nil
          modus-themes-to-toggle          '(modus-vivendi modus-operandi))
  (setopt modus-themes-common-palette-overrides
          '((fg-region unspecified)))
  (setopt modus-vivendi-palette-overrides
          '((bg-mode-line-active        bg-inactive)
            (border-mode-line-active    fg-dim)
            (bg-mode-line-inactive      bg-dim)
            (border-mode-line-inactive  bg-active)))
  (load-theme 'modus-operandi t t)
  (load-theme 'modus-vivendi t t))

(after-init!
  (when (not custom-enabled-themes)
    (require 'modus-themes)
    (load-theme 'modus-operandi t)))

;; Define a dedicated theme `dotemacs' instead of using the default
;; `user' theme to prevent the Emacs customization system from
;; automatically persisting face customizations.
(deftheme dotemacs
  "Central theme for customizing fonts and face properties in this configuration.")

;; Force face settings in this theme to take immediate effect.
(put 'dotemacs 'theme-immediate t)

;; Activate the custom theme immediately after definition
(enable-theme 'dotemacs)

;; Remove `dotemacs' from the list of enabled custom themes to prevent
;; it from being disable when using commands like `consult-theme'.
(delq! 'dotemacs custom-enabled-themes)

(defun dotemacs-theme-refresh (&optional _theme)
  "Customize and set faces for the dotemacs theme.
This function refreshes the dotemacs theme by setting various face
attributes."
  (interactive)
  ;; Temporarily bind `custom--inhibit-theme-enable' to nil to ensure
  ;; the face customizations take effect immediately.
  (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     'dotemacs
     ;; `(default
     ;;   ((((type tty) (class color) (background dark))
     ;;     :background ,(if-let* ((bg (face-attribute 'default :background))
     ;;                            (colors (color-values bg))
     ;;                            ((equal '(0 0 0) colors)))
     ;;                      "unspecified-bg"
     ;;                    bg))))
     `(diff-hl-margin-insert
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-insert)))
     `(diff-hl-margin-delete
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-delete)))
     `(diff-hl-margin-change
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-change)))
     `(diff-hl-margin-ignored
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-ignored)))
     `(diff-hl-margin-unknown
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-unknown)))
     '(secondary-selection
       ((t :foreground unspecified)))
     ;; FIXME It seems that setting :fontset here doesn't work at all.
     ;; term
     '(term
       ((t :family "IosevkaTerm SS04" :fontset "fontset-term")))
     ;; eat
     '(eat-term-font-0
       ((t :family "IosevkaTerm SS04" :fontset "fontset-term"))))))

(defun dotemacs-theme-disable ()
  "Disable the dotemacs theme."
  (interactive)
  (push 'dotemacs custom-enabled-themes)
  (disable-theme 'dotemacs)
  (remove-hook 'server-after-make-frame-hook #'dotemacs-theme-refresh)
  (remove-hook 'enable-theme-functions #'dotemacs-theme-refresh))

(when (daemonp)
  ;; Refresh theme for new frames in server mode
  (add-hook 'server-after-make-frame-hook #'dotemacs-theme-refresh t))

(after-init!
  (dotemacs-theme-refresh))

;; Refresh theme when themes are enabled
(add-hook 'enable-theme-functions #'dotemacs-theme-refresh)

(defvar consult-theme--save-variable t)

(defun consult-theme--state ()
  (let ((saved-theme (car custom-enabled-themes)))
    (lambda (action theme)
      (with-selected-window (or (active-minibuffer-window)
                                (selected-window))
        (let (consult-theme--save-variable)
          (pcase action
            ('return (consult-theme (or theme saved-theme)))
            ((and 'preview (guard theme)) (consult-theme theme))))))))

(with-eval-after-load 'consult
  (with-no-compile!
    (consult-customize consult-theme :state (consult-theme--state))))

(define-advice consult-theme (:after (theme) save)
  "Advice to persist theme selections after using `consult-theme'.
Saves `custom-enabled-themes' to customize settings permanently, and adds
the selected theme's SHA256 hash to `custom-safe-themes' if the theme is not
already marked as safe and is not a built-in default Emacs theme."
  (when consult-theme--save-variable
    (let ((file (locate-file (concat (symbol-name theme) "-theme.el")
                             (custom-theme--load-path)
                             '("" "c"))))
      (unless (or (not file)
                  (eq custom-safe-themes t)
                  (and (memq 'default custom-safe-themes)
                       (equal (file-name-directory file)
                              (expand-file-name "themes/" data-directory))))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((hash (secure-hash 'sha256 (current-buffer))))
            (unless (member hash custom-safe-themes)
              (customize-save-variable 'custom-safe-themes
                                       (cons hash custom-safe-themes))))))
      (customize-save-variable 'custom-enabled-themes custom-enabled-themes
                               "Saved by `consult-theme'."))))

(defun reload-enabled-themes ()
  (interactive)
  (mapc (##load-theme %1 t t)
        custom-enabled-themes))

;;;; Following system theme

(defun dotemacs-theme-follow-toolkit-theme (mode)
  (mapc #'disable-theme custom-enabled-themes)
  (let ((theme (pcase mode
                 ('light 'modus-operandi)
                 ('dark 'modus-vivendi))))
    (enable-theme theme)))

(add-hook 'toolkit-theme-set-functions #'dotemacs-theme-follow-toolkit-theme)
(after-init!
  (dotemacs-theme-follow-toolkit-theme toolkit-theme))

(provide 'dotemacs-theme)
;;; dotemacs-theme.el ends here
