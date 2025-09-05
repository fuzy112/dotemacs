;;; init.el --- Emacs configuration file           -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Zhengyi Fu

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

(require 'early-init early-init-file t)
(when (featurep 'init)
  (load early-init-file nil t))
(eval-when-compile
  (require 'dotemacs-core)
  (require 'compat))

;;;; pre-init.el

(defvar pre-init-file (locate-user-emacs-file "pre-init.el")
  "The file to load before the init file.")

(defvar straight-current-profile)
(when (file-exists-p pre-init-file)
  (let ((straight-current-profile 'user))
    (load pre-init-file nil t)))

;;;; custom

(defvar custom-file)
(when (file-exists-p custom-file)
  (load custom-file))

;;;; meow-edit

(require 'meow)

(defun meow-setup ()
  "Setup meow keybindings for Dvorak layout."
  (setq meow-keypad-leader-dispatch "C-c")
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (setq meow-use-clipboard t)
  (alist-delq! meow-keypad-start-keys ?h)
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("h" . "<help>"))
  ;; (meow-motion-define-key '("<escape>" . ignore))
  (meow-normal-define-key
   '("0"        . meow-expand-0)
   '("9"        . meow-expand-9)
   '("8"        . meow-expand-8)
   '("7"        . meow-expand-7)
   '("6"        . meow-expand-6)
   '("5"        . meow-expand-5)
   '("4"        . meow-expand-4)
   '("3"        . meow-expand-3)
   '("2"        . meow-expand-2)
   '("1"        . meow-expand-1)
   '("-"        . negative-argument)
   '(";"        . meow-reverse)
   '(","        . meow-inner-of-thing)
   '("."        . meow-bounds-of-thing)
   '("<"        . meow-beginning-of-thing)
   '(">"        . meow-end-of-thing)
   '("a"        . meow-append)
   '("A"        . meow-open-below)
   '("b"        . meow-back-word)
   '("B"        . meow-back-symbol)
   '("c"        . meow-change)
   '("d"        . meow-delete)
   '("D"        . meow-backward-delete)
   '("e"        . meow-line)
   '("E"        . meow-goto-line)
   '("f"        . meow-find)
   '("g"        . meow-cancel-selection)
   '("G"        . meow-grab)
   '("h"        . meow-left)
   '("H"        . meow-left-expand)
   '("i"        . meow-insert)
   '("I"        . meow-open-above)
   '("j"        . meow-join)
   '("k"        . meow-kill)
   '("l"        . meow-till)
   '("m"        . meow-mark-word)
   '("M"        . meow-mark-symbol)
   '("n"        . meow-next)
   '("N"        . meow-next-expand)
   '("o"        . meow-block)
   '("O"        . meow-to-block)
   '("p"        . meow-prev)
   '("P"        . meow-prev-expand)
   '("q"        . quit-window)
   '("Q"        . meow-goto-line)
   '("r"        . meow-replace)
   '("R"        . meow-swap-grab)
   '("s"        . meow-search)
   '("t"        . meow-right)
   '("T"        . meow-right-expand)
   '("u"        . meow-undo)
   '("U"        . meow-undo-in-selection)
   '("v"        . meow-visit)
   '("w"        . meow-next-word)
   '("W"        . meow-next-symbol)
   '("x"        . meow-save)
   '("X"        . meow-sync-grab)
   '("y"        . meow-yank)
   '("z"        . meow-pop-selection)
   '("'"        . repeat)
   ;; I used to follow the example.  But binding escape to ignore prevents
   ;; me from quitting vim.
   ;; '("<escape>" . ignore)
   '("("        . meow-backward-slurp)
   '(")"        . meow-forward-slurp)
   '("{"        . meow-backward-barf)
   '("}"        . meow-forward-barf)
   '("`"        . meow-universal-argument)))

(meow-setup)
(alist-setq! meow-replace-state-name-list
  normal "🅝"
  motion "🅼"
  keypad "🅺"
  beacon "🅑"
  insert "🅘")
(defun +meow-setup-indicator ()
  (when meow-global-mode
    (meow-setup-indicator)))
(add-hook 'meow-global-mode-hook #'+meow-setup-indicator)
(meow-global-mode)

(when (treesit-available-p)
  (meow-tree-sitter-register-defaults))

;; Automatically switch to meow-motion-state when magit-blob-mode is
;; active.  This avoids keybinding conflicts between magit-blob-mode
;; and meow-normal-state.
(defun +meow-maybe-switch-to-motion ()
  "Switch to `meow-motion-state' if `magit-blob-mode' is active."
  (when (and meow-global-mode
             (bound-and-true-p magit-blob-mode))
    (meow-motion-mode 1)))

(add-hook 'magit-blob-mode-hook #'+meow-maybe-switch-to-motion)

;;;; repeat-fu

(defun +repeat-fu--meow-mode-h ()
  (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
    (repeat-fu-mode)))
(add-hook 'meow-mode-hook #'+repeat-fu--meow-mode-h)

(defvar-keymap repeat-fu-mode-map
  "C-'" #'repeat-fu-execute)

(after-load! repeat-fu
  (setq repeat-fu-preset 'meow)
  (keymap-set meow-normal-state-keymap "\"" #'repeat-fu-execute))


(after-load! treesit
  (require 'treesit-config))


;;;; straight commands

;; Define a command to run `magit' in package repos.

(eval-when-compile
  (require 'llama))

(defvar straight--recipe-cache)
(defun straight-magit-package-status (pkg)
  "Run `magit-dispatch' in the repo of PKG."
  (interactive
   (list (straight--select-package
          "Visit" (##plist-get %1 :local-repo))))
  (let ((repo (plist-get (gethash pkg straight--recipe-cache)
                         :local-repo)))
    (magit-status-setup-buffer (straight--repos-dir repo))))

(defun +straight-repo-up-to-date-p (&optional strictly)
  (with-temp-buffer
    (process-file "git" nil t nil "rev-list"
                  (if strictly
                      "HEAD...HEAD@{upstream}"
                    "HEAD..HEAD@{upstream}"))
    (zerop (buffer-size))))

(defun +straight-review-updated-repos ()
  "Review repositories with unmerged upstream commits interactively.

This command scans all repositories managed by straight.el and identifies
those with unmerged commits from their upstream branches. It then presents
each such repository in a Magit status buffer sequentially.

During review:
- The mode-line shows progress (current/total) and repository name
- Header displays navigation instructions
- Repository automatically closes when fully synced (during refresh)
- Press \\[exit-recursive-edit] to proceed or \\[abort-recursive-edit] to abort

After processing all repositories, runs `straight-check-all' to rebuild
changed packages."
  (interactive)
  (require 'map)
  (let* ((updated-repos
          (seq-filter (lambda (repo-dir)
                        (let ((default-directory repo-dir))
                          (not (+straight-repo-up-to-date-p))))
                      (seq-map #'straight--repos-dir
                               (map-keys straight--repo-cache))))
         (total (length updated-repos))
         (orig-modeline (default-value 'mode-line-format)))
    (unwind-protect
        (seq-do-indexed
         (lambda (repo-dir index)
           (let* ((default-directory repo-dir)
                  (buf (magit-status-setup-buffer)))
             (unwind-protect
                 (save-window-excursion
                   (add-hook 'kill-buffer-hook #'exit-recursive-edit nil t)
                   (setq-local quit-window-kill-buffer t)
                   (setq mode-line-format
                         (cons (format "(Reviewing %d/%d: %s)"
                                       (1+ index) total
                                       (file-name-nondirectory (directory-file-name repo-dir)))
                               orig-modeline))
                   (setq header-line-format
                         (list (substitute-command-keys "\\[exit-recursive-edit] process next repo, \\[abort-recursive-edit] abort processing")))
                   (add-hook 'magit-pre-refresh-hook
                             (lambda ()
                               (when (+straight-repo-up-to-date-p :strictly)
                                 (exit-recursive-edit)))
                             nil t)
                   (force-mode-line-update)
                   (recursive-edit))
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (kill-local-variable 'kill-buffer-hook))
                 (kill-buffer buf)))))
         updated-repos)
      (redisplay)
      (straight-check-all)
      (message "Finished processing all updated repos"))))

;; Command for fetching all repos asynchronously.
(autoload 'straight-x-fetch-all "straight-x" nil t)

;; Define a prefix keymap for `straight' commands.
(defvar-keymap straight-prefix-map
  :doc    "Prefix map for straight.el commands."
  :prefix 'straight-prefix-map
  "c"     #'straight-check-package
  "C"     #'straight-check-all
  "p"     #'straight-pull-package
  "P"     #'straight-pull-all
  "f"     #'straight-fetch-package
  "F"     #'straight-fetch-all
  "b"     #'straight-rebuild-package
  "B"     #'straight-rebuild-all
  "v"     #'straight-freeze-versions
  "V"     #'straight-thaw-versions
  "n"     #'straight-normalize-package
  "N"     #'straight-normalize-all
  "m"     #'straight-merge-package
  "M"     #'straight-merge-all
  "u"     #'straight-use-package
  "U"     #'+straight-review-updated-repos
  "d"     #'straight-visit-package
  "w"     #'straight-visit-package-website
  "g"     #'straight-magit-package-status
  "r"     #'straight-remove-unused-repos
  "R"     #'straight-prune-build
  "s"     #'straight-push-package
  "S"     #'straight-push-all
  "l"     #'straight-get-recipe
  "L"     #'straight-pull-recipe-repositories
  "x f"   #'straight-x-fetch-all)


;;;; fonts

;; The following fonts need to be installed:
;;  - https://github.com/be5invis/Iosevka/releases/download/v31.8.0/SuperTTC-SGr-IosevkaSS04-31.8.0.zip
;;  - https://github.com/be5invis/Sarasa-Gothic/releases/download/v1.0.26/Sarasa-SuperTTC-1.0.26.7z
;;  - https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/NerdFontsSymbolsOnly.zip

(defvar +fontsets-initilize-hook nil)

(defun +init-fontsets ()
  "Initialize the font configuration"
  ;; Font spec format:
  ;;
  ;; -MAKER-FAMILY-WEIGHT-SLANT-WIDTHTYPE-STYLE...
  ;; ...-PIXELS-HEIGHT-HORIZ-VERT-SPACING-WIDTH-REGISTRY-ENCODING

  ;; Reset the predefined fontsets
  (set-fontset-font "fontset-startup" 'unicode (font-spec :family "Iosevka SS04" :weight 'medium))
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "Iosevka SS04" :weight 'medium))

  (setq use-default-font-for-symbols nil)

  ;; Create custom fontsets
  (dolist (fs '("-*-Iosevka Aile-medium-normal-normal-*-*-*-*-*-*-*-fontset-variable,"
                "-*-Iosevka Fixed Slab-medium-normal-normal-*-*-*-*-*-*-*-fontset-fixed,"
                "-*-IosevkaTerm SS04-medium-normal-normal-*-*-*-*-*-*-*-fontset-term,"))
    (create-fontset-from-fontset-spec fs))

  ;; Customize fonts for CJK characters
  (dolist (script '(han cjk-misc kana hangul bopomofo))
    (set-fontset-font "fontset-default" script "-*-Sarasa Gothic CL-semibold-*" nil 'append)
    (set-fontset-font "fontset-variable" script "-*-Sarasa UI CL-semibold-*")
    (set-fontset-font "fontset-fixed" script "-*-Sarasa Fixed Slab CL-semibold-*")
    (set-fontset-font "fontset-term" script "-*-Sarasa Term CL-semibold-*"))

  ;; Emoji
  (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Color Emoji"))

  ;; Fallback fonts
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "Unifont") nil 'append)
  (set-fontset-font "fontset-default" 'unicode-smp (font-spec :family "Unifont Upper") nil 'append)
  (set-fontset-font "fontset-default" 'unicode-sip (font-spec :family "Unifont Upper") nil 'append)
  (set-fontset-font "fontset-default" 'unicode-ssp (font-spec :family "Unifont Upper") nil 'append)

  ;; Assign fontsets to faces
  (set-frame-font "-*-Iosevka SS04-medium-*" t t) ; NOTE (set-face-attribute 'default nil :fontset "XXX") doesn't work
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :weight 'medium :fontset "fontset-variable")
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Fixed Slab" :weight 'medium :fontset "fontset-fixed")
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Fixed Slab" :weight 'medium :fontset "fontset-fixed")

  (run-hooks '+fontsets-initialize-hook))

(defvar +fontsets-initialized nil)

(defun +maybe-init-fontset ()
  (and (not +fontsets-initialized)
       (display-graphic-p)
       (with-demoted-errors "Failed to setup fonts: %S"
         (+init-fontsets)
         (setq +fontsets-initialized t))))

(+maybe-init-fontset)
(unless +fontsets-initialized
  (add-hook 'server-after-make-frame-hook #'+maybe-init-fontset))

(setq xft-ignore-color-fonts nil
      face-ignored-fonts nil)

;;;; themes

(defun +inhibit-implied-resize (&rest args)
  "Prevent implied resize when switching themes."
  (let ((frame-inhibit-implied-resize t))
    (apply args)))

(advice-add 'enable-theme :around #'+inhibit-implied-resize)
(advice-add 'disable-theme :around #'+inhibit-implied-resize)

;;;; modus-theme

(setq modus-themes-mixed-fonts        t
      modus-themes-bold-constructs    t
      modus-themes-slanted-constructs t
      modus-themes-variable-pitch-ui  nil
      modus-themes-to-toggle          '(modus-vivendi modus-operandi))

(after-load! modus-themes
  (setopt modus-themes-common-palette-overrides modus-themes-preset-overrides-faint))

(when (not custom-enabled-themes)
  (require-theme 'modus-themes)
  (modus-themes-load-theme 'modus-operandi))

;; Define a dedicated theme `dotemacs' instead of using the default
;; `user' theme to prevent the Emacs customization system from
;; automatically persisting face customizations.
(deftheme dotemacs
  "Central theme for customizing fonts and face properties in this configuration.")

;; Activate the custom theme immediately after definition
(enable-theme 'dotemacs)

;; Remove `dotemacs' from the list of enabled custom themes to prevent
;; it from being disable when using commands like `consult-theme'.
(setq custom-enabled-themes (remq 'dotemacs custom-enabled-themes))

(defun dotemacs-theme-refresh (&optional _theme)
  "Customize and set faces for the dotemacs theme.
This function refreshes the dotemacs theme by setting various face
attributes."
  ;; Temporarily bind `custom--inhibit-theme-enable' to nil to enuser
  ;; the face customizations take effect immediately.
  (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     'dotemacs
     `(cursor
       ((((class color) (min-colors 256) (background light)) :background "#005077")
        (((class color) (min-colors 256) (background dark)) :background "#40c8ec")))
     `(fill-column-indicator
       ((((type w32 tty))
         :height 1.0 :foreground "gray50" :background ,(face-background 'default))))
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
     '(whitespace-indentation ((t :underline "yellow")))
     '(whitespace-space-before-tab ((t :underline "DarkOrange")))
     '(whitespace-space-after-tab ((t :underline "yellow")))
     '(parenthesis
       ((t :inherit shadow)))
     `(header-line
       ((((supports :underline t) (class color grayscale))
         :background ,(face-background 'default)
         :underline ( :color ,(face-foreground 'default)
                      :style line
                      :position t)
         :box (:line-width 6 :style flat-button))))
     `(header-line-inactive
       ((t :inherit (shadow header-line))))
     `(mode-line-active
       ((((supports :overline t) (class color grayscale))
         :background ,(face-background 'default)
         :foreground ,(face-foreground 'default)
         :overline ,(face-foreground 'default)
         :box ( :line-width 6
                :color ,(face-background 'default)
                :style nil))))
     `(mode-line-inactive
       ((((supports :overline t) (class color grayscale))
         :background ,(face-background 'default)
         :foreground ,(face-foreground 'shadow)
         :overline t
         :box ( :line-width 6
                :color ,(face-background 'default)
                :style nil))))
     `(tab-line-tab-current
       ((((min-colors 256) (background dark))
         :background "purple"
         :foreground "white"
         :box nil)))
     `(tab-line-tab-inactive
       ((((min-colors 256) (background dark))
         :background "DarkGreen"
         :foreground "white"
         :box nil)))
     `(tab-bar
       ((((supports :box t))
         :box ( :line-width (-2 . 6)
                :style flat-button))))
     '(region
       ((t :foreground unspecified)))
     '(secondary-selection
       ((t :foreground unspecified)))
     '(minibuffer-depth-indicator
       ((t :inherit shadow)))
     ;; FIXME It seems that setting :fontset here doesn't work at all.
     ;; term
     '(term
       ((t :family "IosevkaTerm SS04" :fontset "fontset-term")))
     ;; eat
     '(eat-term-font-0
       ((t :family "IosevkaTerm SS04" :fontset "fontset-term"))))))

(if (daemonp)
    ;; Refresh theme for new frames in server mode
    (add-hook 'server-after-make-frame-hook #'dotemacs-theme-refresh t)
  ;; Refresh theme immediately
  (dotemacs-theme-refresh))

;; Refresh theme when themes are enabled
(add-hook 'enable-theme-functions #'dotemacs-theme-refresh)

;;;; libraries
(after-load! (:or dash elisp-mode)
  (global-dash-fontify-mode))
(after-load! info-look
  (dash-register-info-lookup))
(after-load! (:and anaphora elisp-mode)
  (anaphora-install-font-lock-keywords))

;;;; nerd-icons

(add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(add-hook 'marginalia-mode-hook    #'nerd-icons-completion-mode)
(add-hook 'ibuffer-mode-hook       #'nerd-icons-ibuffer-mode)
(add-hook 'dired-mode-hook         #'nerd-icons-multimodal-mode)
(add-hook 'archive-mode-hook       #'nerd-icons-multimodal-mode)
(add-hook 'tar-mode-hook           #'nerd-icons-multimodal-mode)
(add-hook 'grep-mode-hook          #'nerd-icons-grep-mode)

;; show nerd-icons on mode-line
(setq-default mode-line-buffer-identification
              (seq-union '((:eval (nerd-icons-icon-for-buffer)) " ")
                         mode-line-buffer-identification))

(after-load! nerd-icons
  (when (display-graphic-p)
    (nerd-icons-set-font))
  (add-hook '+fontsets-initialize-hook #'nerd-icons-set-font)
  (add-hook 'server-after-make-frame-hook #'nerd-icons-set-font))

;;;; ultra-scroll

(setq scroll-conservatively 101)
(if (and (featurep 'x) (not (featurep 'xinput2)))
    (pixel-scroll-precision-mode)
  (ultra-scroll-mode))

;;;; window

(setopt kill-buffer-quit-windows t
        quit-restore-window-no-switch t
        frame-auto-hide-function #'delete-frame)

(when (and (boundp 'quit-window-kill-buffer) (listp quit-window-kill-buffer))
  (setq quit-window-kill-buffer
        (seq-union quit-window-kill-buffer
                   (list 'magit-diff-mode 'magit-revision-mode
                         'xref--xref-buffer-mode 'ediff-mode))))

;;;; tab-bar

(setq tab-bar-tab-name-format-function
      (lambda (tab i)
        (tab-bar-tab-name-format-face
         (concat " "
                 (propertize (number-to-string i) 'face '(:weight ultra-bold :underline t))
                 " "
                 (alist-get 'name tab)
                 " ")
         tab i)))

;;;; quick-window

(autoload 'quick-window-jump "quick-window" nil t)
(keymap-global-set "M-o" #'quick-window-jump)

;;;; help

(after-load! help
  ;; Enable editing of variable values in help buffers
  (setq help-enable-variable-value-editing t
        help-enable-completion-autoload nil
        help-window-select t
        help-window-keep-selected t)

  ;; Add shortdoc examples to function help if available
  (when (fboundp 'shortdoc-help-fns-examples-function)
    (add-hook 'help-fns-describe-function-functions
              #'shortdoc-help-fns-examples-function 50))

  ;; Insert the source code of the function in `describe-function' buffers.
  (add-to-list 'help-fns-describe-function-functions #'help-fns-function-source-code 'append))

(defun help-fns-function-source-code (function)
  "Insert the Emacs Lisp source code for FUNCTION into current buffer.
FUNCTION should be a symbol naming a function.  The source code is
extracted from the function's definition and inserted with proper
font-locking and indentation."
  (when-let* ((position (ignore-errors
                          (let ((inhibit-interaction t))
                            (find-function-noselect function))))
              (buffer (car position))
              (point (cdr position))
              (text (with-current-buffer buffer
                      (save-excursion
                        (goto-char point)
                        (end-of-defun)
                        (let ((end (point)))
                          (beginning-of-defun)
                          (font-lock-ensure (point) end)
                          (buffer-substring (point) end))))))
    ;; Add indentation properties to make the source code align properly
    (add-text-properties 0 (length text)
                         '(line-prefix (space :align-to 2))
                         text)
    ;; Insert the source code with section header
    (insert "\n  Source code:\n\n")
    (insert text)
    (insert "\n\n")))

(defun +mail-to-help-gnu-emacs ()
  (interactive)
  (compose-mail
   "help-gnu-emacs@gnu.org"
   (read-string "Title: ")))

(after-load! help
  (keymap-set help-map "H" #'+mail-to-help-gnu-emacs))

;;;; backup

(autoload 'list-backups "backup" nil t)
(autoload 'backup-list-backups "backup" nil t)

;;;; breadcrumb

;; Enable breadcrumb-local-mode in various major modes
(add-hook 'text-mode-hook #'breadcrumb-local-mode)    ; Enable for text files
(add-hook 'conf-mode-hook #'breadcrumb-local-mode)    ; Enable for configuration files
(add-hook 'prog-mode-hook #'breadcrumb-local-mode)    ; Enable for programming modes

;;;; orderless

(eval-when-compile (require 'orderless))

;; Define two “compound” completion styles on top of orderless and give
;; them convenient names so that they can be re-used in
;; completion-category-overrides without repeating a long list every
;; time.

;; orderless+flex enables “flex” (aka scatter) matching, useful for
;; symbols where short substrings may suffice – e.g. typing “baf” →
;; “buffer-auto-save-file-name”.
(orderless-define-completion-style orderless+flex
  (orderless-matching-styles '(orderless-flex)))

;; orderless+initialism is tuned for commands and variables where
;; initialisms are common.  The style tries (in order):
;;   1. exact initialism match (“tb” → “toggle-button”),
;;   2. literal substring,
;;   3. regexp.
(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))

;; --- Global defaults -------------------------------------------------
;; Search for candidates with orderless first, fall back to the “basic”
;; style (Emacs standard prefix + substring) when orderless returns no
;; matches.
(setq completion-styles '(orderless basic))

;; Let categories decide everything, do not force any extra defaults.
(setq completion-category-defaults nil)

;; --- Category-specific tweaks ----------------------------------------
;; Different kinds of completions benefit from different matching
;; behaviour, so override the style on a per-category basis:
(setq completion-category-overrides
      '(
        ;; For file names the default `basic' already deals with partial
        ;; paths (“~/.e” → “~/.emacs.d/”), keep it.
        (file        . ((styles . (basic partial-completion))))

        ;; Make symbols and symbol-help use flexible matching.
        (symbol      . ((styles . (orderless+flex))))
        (symbol-help . ((styles . (orderless+flex))))

        ;; Commands and variables are represented mostly by their
        ;; command/variable names – initialism matching shines here.
        (command     . ((styles . (orderless+initialism))))
        (variable    . ((styles . (orderless+initialism))))

        ;; Eglot completion and its capf backend: just plain orderless,
        ;; no extra tweaks.
        (eglot       . ((styles . (orderless))))
        (eglot-capf  . ((styles . (orderless))))

        ;; Git revisions (magit-rev) are often typed by scattered parts
        ;; (“mwr” → “merge-request-work”), so fall back to flex.
        (magit-rev   . ((styles . (orderless+flex))))))


;; Registers an orderless dispatcher that makes
;; 1) the “$” suffix work together with Consult’s tofu suffixes, and
;; 2) dotted file-extension patterns of the form “..EXT”.

(defun +orderless--consult-suffix ()
  "Return a regexp that matches either:
  - the end of string “$”, or
  - any sequence of Consult tofu characters followed by the end of string."
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$" consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

(defun +orderless--consult-dispatch (word _index _total)
  "Transform WORD for orderless, producing a cons cell (STYLE . REGEXP).
Leaves WORD untouched unless it is one of the patterns handled specially here:
  - WORD ends with “$”           → remove trailing “$” and re-append the suffix
  - WORD starts with “..EXT”      → turn it into a \\.EXT.*<suffix> pattern
The return value is nil when WORD does not match either pattern, so orderless
falls back to its default handling."
  (cond
   ;; Allow completing-read users to terminate a string with “$” even when
   ;; Consult appends tofu suffixes such as “<taboo char>…index”.
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
   ;; In file-name contexts, let “..ext” expand to any file whose name ends in
   ;; the extension “.ext”, hiding files that do not end with that extension.
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

(after-load! orderless
  ;; The dispatcher must be added *after* orderless has been loaded;
  ;; otherwise orderless-style-dispatchers may not be defined yet.
  (add-to-list 'orderless-style-dispatchers #'+orderless--consult-dispatch))

;;;; vertico


;; Allow recursive minibuffers, so commands invoked from within the minibuffer
;; (e.g., C-x C-f followed by M-:) can themselves use the minibuffer.
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
(setq minibuffer-depth-indicator-function
      (lambda (depth)
        (propertize
         (concat (make-string depth ?⮐) " ")
         'face 'minibuffer-depth-indicator)))

;; Autoload Vertico's main advice function and apply it around the two core
;; completing-read entry points, ensuring Vertico is used everywhere Emacs
;; prompts for completions.
(autoload 'vertico--advice "vertico")
(advice-add #'completing-read-default :around #'vertico--advice)
(advice-add #'completing-read-multiple :around #'vertico--advice)

(after-load! vertico
  (require 'orderless)
  (setq vertico-quick1 "htnsd"
        vertico-quick2 "ueoai")
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode)
  (keymap-global-set "M-R" #'vertico-repeat)
  (define-keymap :keymap vertico-map
    ;; vertico-repeat
    "M-P"   #'vertico-repeat-previous
    "M-N"   #'vertico-repeat-next
    ;; vertico-directory
    "RET"   #'vertico-directory-enter
    "DEL"   #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    ;; vertico-quick
    "C-q"   #'vertico-quick-insert
    "M-q"   #'vertico-quick-exit))

;;;; marginalia

(autoload 'marginalia--minibuffer-setup "marginalia")
(add-hook 'minibuffer-setup-hook #'marginalia--minibuffer-setup)
(keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)

(after-load! marginalia
  (alist-setq! marginalia-prompt-categories
    "\\<info manuals\\>" 'info-manual
    "\\<manual name\\>" 'info-manual
    "\\<Log rev,s\\>" 'magit-rev)
  (marginalia-mode))

;;;; crm

(defvar crm-separator)
(when (version< emacs-version "31.1")
  (define-advice completing-read-multiple
      (:filter-args (args) show-crm-separator)
    "Add prompt indicator to `completing-read-multiple'.
ARGS: see `completion-read-multiple'."
    (cons (format "[`CRM': %s]  %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args))))

;;;; cursor-sensor

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Prevent typing before the prompt.
(setq minibuffer-prompt-properties '(read-only t face minibuffer-prompt cursor-intangible t))

;;;; pulsar

(when (display-graphic-p)
  (pulsar-global-mode))

;;;; goggles

(add-hook 'prog-mode-hook #'goggles-mode)
(add-hook 'text-mode-hook #'goggles-mode)

;;;; corfu

(setopt completion-cycle-threshold nil
        tab-always-indent 'complete
        text-mode-ispell-word-completion nil
        read-extended-command-predicate #'command-completion-default-include-p)

(define-advice completion-in-region (:before (&rest _) corfu)
  (require 'corfu))

(autoload 'corfu--minibuffer-on "corfu")
(add-hook 'minibuffer-setup-hook #'corfu--minibuffer-on 100)

(defvar corfu-map)
(after-load! corfu
  (advice-remove 'completion-in-region #'completion-in-region@corfu)
  (require 'orderless)
  (setopt corfu-cycle t
          corfu-preselect 'prompt)
  (setopt corfu-quick1 "htnsd"
          corfu-quick2 "ueoai")
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (define-keymap :keymap corfu-map
    "C-q"       #'corfu-quick-insert
    "M-q"       #'corfu-quick-complete
    "TAB"       #'corfu-next
    "S-TAB"     #'corfu-previous
    "<backtab>" #'corfu-previous))

(unless (featurep 'tty-child-frames)
  (add-hook 'tty-setup-hook #'corfu-terminal-mode)
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

;;;; cape

(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)

;;;; dabbrev

(defvar dabbrev-ignored-buffer-regexps)
(defvar dabbrev-ignored-buffer-modes)
(after-load! dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;;;; tempel

(keymap-global-set "M-+" #'tempel-complete)
(keymap-global-set "M-*" #'tempel-insert)
(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))
(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(declare-function tempel--templates "tempel.el")
(defun tempel-include (elt)
  (when (eq (car-safe elt) 'i)
    (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))
(after-load! tempel
  (setq tempel-path (concat user-emacs-directory "/templates/*.eld"))
  (add-to-list 'tempel-user-elements #'tempel-include))

;;;; embark

(keymap-global-set "C-." #'embark-act)
(keymap-global-set "C-c a" #'embark-act)
(setq prefix-help-command #'embark-prefix-help-command)

(define-advice embark-dwim (:before (&rest _args) mouse)
  (when (mouse-event-p last-command-event)
    (mouse-set-point last-command-event)))

(defun +embark/find-file-as-root (file)
  "Find FILE as root."
  (interactive "fFind file as root: ")
  (let* ((absolute-file-name (expand-file-name file))
         (remote (file-remote-p absolute-file-name))
         sudo-file-name)
    (cond (remote
           (let ((method (file-remote-p absolute-file-name 'method))
                 (user (file-remote-p absolute-file-name 'user))
                 (host (file-remote-p absolute-file-name 'host))
                 (localname (file-remote-p absolute-file-name 'localname)))
             (when (equal method "scp")
               (setq method "ssh"))
             (if (or (equal user "root") (equal method "sudo"))
                 (setq sudo-file-name absolute-file-name)
               (setq sudo-file-name (concat "/" method ":" user "@" host "|sudo::" localname)))))
          (t (setq sudo-file-name (concat "/sudo::" absolute-file-name))))
    (find-file sudo-file-name)))

(declare-function bookmark-prop-get "bookmark.el")
(defun +embark/eww-open-bookmark (bookmark)
  "Open BOOKMARK with `eww'."
  (eww (or (bookmark-prop-get bookmark 'location)
           (bookmark-prop-get bookmark 'filename)
           (user-error "Bookmark `%s' doesn't have a location" bookmark))))

(defun +embark/browse-url-open-bookmark (bookmark)
  "Open BOOKMARK with `browse-url'."
  (browse-url (or (bookmark-prop-get bookmark 'location)
                  (bookmark-prop-get bookmark 'filename)
                  (user-error "Bookmark `%s' doesn't have a location" bookmark))))

(defun +embark/apply-ansi-color (beg end &optional use-overlays)
  "Apply ansi color sequence on the text from BEG to END.

When interactive, prefix-argument means to use overlays instead of text
properties, if supported.  Elisp code can achieve this with non-nil
value for USE-OVERLAYS."
  (interactive "*r\nP")
  (if (fboundp 'xterm-color-colorize-buffer)
      (save-restriction
        (narrow-to-region beg end)
        (xterm-color-colorize-buffer use-overlays))
    (eval-and-compile (require 'ansi-color))
    (let ((ansi-color-apply-face-function
           (if use-overlays #'ansi-color-apply-overlay-face
             #'ansi-color-apply-text-property-face)))
      (ansi-color-apply-on-region beg end))))

(after-load! embark
  (setq embark-cycle-key "C-.")
  (setq embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  (keymap-set embark-file-map "#" '+embark/find-file-as-root)
  (keymap-set embark-bookmark-map "W" '+embark/eww-open-bookmark)
  (keymap-set embark-bookmark-map "u" '+embark/browse-url-open-bookmark)
  (keymap-set embark-region-map "[" '+embark/apply-ansi-color))

;;;; consult

(autoload 'consult-ugrep "consult-ugrep" nil t)

(define-keymap
  :keymap global-map
  "C-c M-x"               #'consult-mode-command
  "C-c H"                 #'consult-history
  "C-c k"                 #'consult-kmacro
  "C-c m"                 #'consult-man
  "C-c i"                 #'consult-info
  "<remap> <Info-search>" #'consult-info
  ;; C-x bindings in `ctl-x-map'
  "C-x M-:"               #'consult-complex-command     ; orig. repeat-complex-command
  "C-x b"                 #'consult-buffer              ; orig. switch-to-buffer
  "C-c b"                 #'consult-buffer
  "C-x 4 b"               #'consult-buffer-other-window ; orig. switch-to-buffer-other-window
  "C-x 5 b"               #'consult-buffer-other-frame  ; orig. switch-to-buffer-other-frame
  "C-x t b"               #'consult-buffer-other-tab    ; orig. switch-to-buffer-other-tab
  "C-x r b"               #'consult-bookmark            ; orig. bookmark-jump
  "C-x p b"               #'consult-project-buffer      ; orig. project-switch-to-buffer
  ;; Custom M-            # bindings for fast register access
  "M-#"                   #'consult-register-load
  "M-'"                   #'consult-register-store      ; orig. abbrev-prefix-mark (unrelated)
  "C-M-#"                 #'consult-register
  ;; Other custom bindings
  "M-y"                   #'consult-yank-pop            ; orig. yank-pop
  ;; M-g bindings in `goto-map'
  "M-g e"                 #'consult-compile-error
  "M-g f"                 #'consult-flymake             ; Alternative: consult-flycheck
  "M-g g"                 #'consult-goto-line           ; orig. goto-line
  "M-g M-g"               #'consult-goto-line           ; orig. goto-line
  "M-g o"                 #'consult-outline             ; Alternative: consult-org-heading
  "M-g m"                 #'consult-mark
  "M-g k"                 #'consult-global-mark
  "M-g i"                 #'consult-imenu
  "M-g I"                 #'consult-imenu-multi
  ;; M-s bindings in `search-map'
  "M-s d"                 #'consult-find
  "M-s D"                 #'consult-fd
  "M-s c"                 #'consult-locate
  "M-s g"                 #'consult-grep
  "M-s G"                 #'consult-git-grep
  "M-s r"                 #'consult-ripgrep
  "M-s R"                 #'consult-ugrep
  "M-s l"                 #'consult-line
  "M-s L"                 #'consult-line-multi
  "M-s k"                 #'consult-keep-lines
  "M-s u"                 #'consult-focus-lines
  ;; Isearch integration
  "M-s e"                 #'consult-isearch-history)

(define-keymap :keymap isearch-mode-map
  "M-e"   #'consult-isearch-history
  "M-s e" #'consult-isearch-history
  "M-s l" #'consult-line
  "M-s L" #'consult-line-multi)

(define-keymap :keymap minibuffer-local-map
  "M-s" #'consult-history
  "M-r" #'consult-history)

(after-load! consult
  (remove-hook 'consult-after-jump-hook #'recenter)
  (add-hook 'consult-after-jump-hook #'reposition-window)

  (add-to-list 'consult-buffer-filter "\\`\\*EGLOT")
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode))

(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)


;; Declare internal functions of consult to avoid bytecomp warnings.
(declare-function consult--read-1 "consult.el" (arg1 &rest rest))
(declare-function consult--file-preview "consult.el")
(declare-function consult--buffer-preview "consult.el")

;; Add preview for `read-file-name'.
(defun +consult--read-file-name-function (prompt &optional dir _default mustmatch initial pred)
  (let* ((default-directory (abbreviate-file-name (or dir default-directory)))
         (minibuffer-completing-file-name t)
         (pred (or pred 'file-exists-p)))
    (require 'consult)
    (consult--read-1
     #'read-file-name-internal
     :state (consult--file-preview)
     :prompt prompt
     :initial (if initial
                  (expand-file-name initial)
                default-directory)
     :require-match mustmatch
     :predicate pred
     :preview-key "M-."
     :sort t
     :lookup (lambda (selected &rest _) (substitute-in-file-name selected)))))

(setq read-file-name-function #'+consult--read-file-name-function)

;; Add preview for `read-buffer'.

(defvar consult-preview-key)
(defun +consult--read-buffer-function (prompt &optional def mustmatch pred)
  (require 'consult)
  (consult--read-1
   #'internal-complete-buffer
   :state         (consult--buffer-preview)
   :default       def
   :prompt        (format-prompt (replace-regexp-in-string ":[[:space:]]*\\'" "" prompt) def)
   :require-match mustmatch
   :predicate     pred
   :preview-key   consult-preview-key
   :sort          t
   :lookup        (lambda (selected &rest _) selected)))

(setq read-buffer-function #'+consult--read-buffer-function)


;; Use `orderless-compile' as the `consult''s default regexp compiler.

(declare-function orderless-compile "orderless.el" (arg1 &optional arg2 arg3))
(declare-function orderless--highlight "orderless.el" (arg1 arg2 arg3))
(declare-function consult--convert-regexp "consult.el" (arg1 arg2))

(defvar orderless-match-faces)
(defun +consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (cdr (orderless-compile input)))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str)
     (let ((orderless-match-faces orderless-match-faces))
       (setq orderless-match-faces (vconcat '(consult-highlight-match) orderless-match-faces))
       (orderless--highlight input t str)))))

(after-load! consult
  (setq consult-preview-key "M-.")
  (setq consult-narrow-key "<") ;; "C-+"
  (setq-default consult--regexp-compiler #'+consult--orderless-regexp-compiler)

  ;; consult-customize is a macro and is not autoloaded
  (with-no-compile!
   (consult-customize
    consult-xref consult-ripgrep consult-grep consult-git-grep
    consult-line consult-focus-lines consult-keep-lines
    consult-imenu
    :preview-key '(:debounce 0.2 any)))

  ;; url-only bookmark type
  (cl-pushnew #'url-bookmark-jump (cddr (assoc ?w consult-bookmark-narrow))))


(defun +recenter-top-30% ()
  (recenter (ceiling (* (window-height) 0.3))))
(defun +recenter-bottom-30% ()
  (recenter (ceiling (* (window-height) 0.7))))


;; Keybindings for `consult-dir' commands.

(keymap-global-set "C-x C-d" #'consult-dir)
(define-keymap :keymap minibuffer-local-map
  "C-x C-d" #'consult-dir
  "C-x C-j" #'consult-dir-jump-file)


;; Custom consult commands.
(autoload 'consult-kill "consult-kill" nil t)


(defun send-password-to-process (process)
  "Read a password and send it to the process in BUFFER."
  (interactive
   (list
    (if-let* ((proc (get-buffer-process (current-buffer)))
              ((not current-prefix-arg)))
        proc
      (read-process-name "Process: "))))
  (process-send-string process
                       (concat
                        (read-passwd "Password: ")
                        "\n")))

;;;; indent-aux

;; New minor mode in Emacs 30.1: deindents code copied into kill-ring.
(when (fboundp 'kill-ring-deindent-mode)
  (kill-ring-deindent-mode))

;;;; windmove

(define-keymap :keymap global-map
  "S-<left>"  #'windmove-left
  "S-<right>" #'windmove-right
  "S-<up>"    #'windmove-up
  "S-<down>"  #'windmove-down)

;;;; ibuffer

;; Replace `list-buffers' with `ibuffer-jump'.
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-jump)

(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-default-sorting-mode 'filename/process
      ibuffer-use-header-line t
      ibuffer-default-shrink-to-minimum-size nil)

(setq ibuffer-saved-filter-groups
      '(("Main"
         ("Directories" (mode . dired-mode))
         ("C++" (or
                 (mode . c++-mode)
                 (mode . c++-ts-mode)
                 (mode . c-mode)
                 (mode . c-ts-mode)
                 (mode . c-or-c++-ts-mode)))
         ("Python" (or
                    (mode . python-mode)
                    (mode . python-ts-mode)))
         ("Build" (or
                   (mode . make-mode)
                   (mode . amkefile-gmake-mode)
                   (name . "\\`Makefile\\'")
                   (mode . change-log-mode)))
         ("Scripts" (or
                     (mode . shell-script-mode)
                     (mode . shell-mode)))
         ("Config" (or
                    (mode . conf-mode)
                    (mode . conf-toml-mode)
                    (mode . toml-ts-mode)
                    (mode . toml-mode)
                    (mode . yaml-mode)
                    (mode . yaml-ts-mode)
                    (mode . json-ts-mode)
                    (mode . js-json-mode)
                    (mode . gitconfig-mode)))
         ("Web" (or
                 (mode . html-mode)
                 (mode . web-mode)
                 (mode . nxml-mode)))
         ("Markup" (or
                    (mode . markdown-mode)
                    (mode . markdown-ts-mode)
                    (mode . adoc-mode)
                    (mode . rst-mode)))
         ("Org-mode" (mode . org-mode))
         ("Magit" (or
                   (mode . magit-mode)
                   (mode . magit-log-mode)
                   (mode . magit-blame-mode)
                   (mode . magit-cherry-mode)
                   (mode . magit-diff-mode)
                   (mode . magit-process-mode)
                   (mode . magit-status-mode)))
         ("Apps" (or
                  (mode . elfeed-search-mode)
                  (mode . elfeed-show-mode)
                  (mode . eww-mode)
                  (mode . telega-chat-mode)
                  (mode . telega-root-mode)
                  (mode . telega-webpage-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)))
         ("Doc" (or
                 (mode . Info-mode)
                 (mode . Man-mode)
                 (mode . Woman-mode)
                 (mode . devdocs-mode)
                 (mode . good-doc-mode)))
         ("Terminal" (or
                      (mode . term-mode)
                      (mode . eat-mode)
                      (mode . vterm-mode)))
         ("AI" (or
                (mode . gptel-mode)
                (name . "\\`<eca.*:.+>\\'")))
         ("Emacs" (or
                   (mode . emacs-lisp-mode)
                   (mode . help-mode)
                   (name . "\\*Messages\\*")
                   (name . "\\`\\.newsrc-dribble\\'")
                   (name . "\\*Completions\\*")
                   (mode . lisp-interaction-mode)
                   (name . "\\*log-edit-files\\*")
                   (name . "\\*Process List\\*"))))))

(defun ibuffer-config-saved-filter-groups ()
  (ibuffer-switch-to-saved-filter-groups "Main"))

(add-hook 'ibuffer-mode-hook #'ibuffer-config-saved-filter-groups)

;;;; apheleia

(keymap-global-set "C-x x /" #'apheleia-format-buffer)
(keymap-global-set "C-c C-/" #'apheleia-format-buffer)

;;;; ws-butler

(autoload 'ws-butler--global-mode-turn-on "ws-butler")
(add-hook 'find-file-hook #'ws-butler--global-mode-turn-on)
(after-load! ws-butler
  (remove-hook 'find-file-hook #'ws-butler--global-mode-turn-on)
  (setopt ws-butler-keep-whitespace-before-point nil
          ws-butler-convert-leading-tabs-or-spaces nil)
  (add-to-list 'ws-butler-global-exempt-modes 'diff-mode)
  (ws-butler-global-mode))

;;;; whitespace

(dolist (hook '(prog-mode-hook conf-mode-hook yaml-mode-hook))
  (add-hook hook #'whitespace-mode))
(setopt whitespace-style '(face
                           trailing empty indentation
                           space-before-tab space-after-tab
                           missing-newline-at-eof))

;;;; indent-tabs-mode

;; Guess whether `indent-tabs-mode' should be enabled by looking at
;; the first 10000 lines.
(defun +indent-tabs-mode--hack-local-variables-h ()
  (unless (derived-mode-p 'special-mode
                          'markdown-mode 'markdown-ts-mode 'org-mode
                          'makefile-mode
                          'diff-mode
                          'term-mode 'eshell-mode 'eat-mode 'comint-mode
                          'minibuffer-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((score 0))
        (while (re-search-forward (format "^\\(\t\\| \\{%d\\}\\)" tab-width) 10000 t)
          (if (equal (match-string 1) "\t")
              (cl-incf score)
            (cl-decf score)))
        (unless (zerop score)
          (indent-tabs-mode score))))))
(add-hook 'hack-local-variables-hook #'+indent-tabs-mode--hack-local-variables-h)

;;;; recentf
(setq recentf-save-file (locate-user-emacs-file '("recentf.eld.zst")))

(autoload 'recentf-track-opened-file "recentf"
  "Insert the name of the file just opened or written into the recent list." )
(add-hook 'find-file-hook #'recentf-track-opened-file)
;; (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)
(after-load! recentf
  (setq recentf-max-saved-items 8192)
  (let ((inhibit-message t))
    (recentf-mode)))
(after-load! consult
  ;; enable recentf when accessing the recentf-file source from consult.
  (setf (plist-get consult--source-recent-file :enabled)
        (lambda () (require 'recentf) (symbol-value 'recentf-mode))))

;;;; saveplace

(setq save-place-file
      (locate-user-emacs-file '("places.eld.zst")))
(setq save-place-limit 65536)
(autoload 'save-place-find-file-hook "saveplace")
(autoload 'save-place-dired-hook "saveplace")
(add-hook 'find-file-hook #'save-place-find-file-hook)
(add-hook 'dired-initial-position-hook #'save-place-dired-hook)
(after-load! saveplace
  (save-place-mode))

;;;; autorevert

(autoload 'auto-revert--global-adopt-current-buffer "autorevert")
(add-hook 'find-file-hook #'auto-revert--global-adopt-current-buffer)

(after-load! autorevert
  (global-auto-revert-mode))

;;;; dired

(add-hook 'dired-mode-hook #'dired-omit-mode)
(after-load! dired
  (setq dired-listing-switches "-lah"
        dired-hide-details-hide-absolute-location t
        dired-do-revert-buffer t
        dired-x-hands-off-my-keys nil))

(defun +dired-side-noselect ()
  "Open a dired buffer in a side window. "
  (interactive)
  (let ((buf (dired-noselect (and (project-current) (project-root (project-current))))))
    (with-current-buffer buf
      (make-local-variable 'display-buffer-alist)
      (cl-pushnew '((derived-mode . dired-mode) (display-buffer-same-window)) display-buffer-alist :test 'equal)
      (dired-hide-details-mode)
      (run-hooks '+dired-side-hook))
    buf))

(defun +dired-side ()
  (interactive)
  (display-buffer
   (+dired-side-noselect)
   `((display-buffer-in-side-window)
     (side . left)
     (dedicated . t)
     (window-width . 40)
     (window-parameters . ((dired-side . t))))))

(add-hook '+dired-side-hook (lambda () (tab-line-mode -1)))

(defun +dired-side-goto-current-file (frame)
  (with-selected-frame frame
    (when-let* ((file (buffer-file-name))
                (side-win (window-with-parameter 'dired-side  nil frame))
                (buf (window-buffer side-win)))
      (with-current-buffer buf
        (dired-insert-subdir (file-name-directory file))
        (dired-goto-file file)
        (recenter)
        (hl-line-highlight)))))

;; (add-hook 'window-buffer-change-functions #'+dired-side-goto-current-file)

;;;; compile

(after-load! compile
  (setq compilation-always-kill t
        compilation-ask-about-save t
        compilation-scroll-output 'first-error))

;;;; comint

(after-load! comint
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048))

;;;; grep

(setq grep-use-headings t)

;;;; eshell

(defun +eshell/other-window ()
  "Open an EShell buffer in other window."
  (interactive)
  (with-suppressed-warnings ((obsolete display-comint-buffer-action))
    (defvar display-comint-buffer-action)
    (let ((display-comint-buffer-action '(() (inhibit-same-window . t)))
          (display-buffer-alist (cons '((category . comint)
                                        nil
                                        (inhibit-same-window . t))
                                      display-buffer-alist)))
      (eshell))))

(defvar eshell-hist-mode-map)
(after-load! eshell
  (setopt eshell-scroll-to-bottom-on-input 'this
          eshell-history-size 8192
          eshell-save-history-on-exit t)
  (setopt eshell-modules-list (seq-union '(eshell-tramp) (symbol-value 'eshell-modules-list)))
  (defun +eshell--capf ()
    (when (fboundp 'cape-history)
      (add-hook 'completion-at-point-functions #'cape-history 50 t)))
  (add-hook 'eshell-mode-hook '+eshell--capf)
  (after-load! esh-hist
    (when (fboundp 'consult-history)
      (keymap-set eshell-hist-mode-map "M-r" #'consult-history)))
  (after-load! em-term
   (cl-pushnew "journalctl" eshell-visual-commands :test #'equal)
   (alist-setq! eshell-visual-subcommands
     "git" '("log" "diff" "show" "grep")
     "systemctl" '("status" "cat" "show")))
  (require 'eshell-extras))

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

(autoload 'cmake-capf-setup "cmake-capf")
(add-hook 'cmake-mode-hook #'cmake-capf-setup)
(add-hook 'cmake-ts-mode-hook #'cmake-capf-setup)

;;;; qmake

(alist-setq! auto-mode-alist
  "\\.pr[oi]\\'" 'makefile-mode)

;;;; systemd-mode

(autoload 'flymake-systemd "flymake-systemd"
  "Verify the systemd unit file.")

(defun +systemd-mode--setup ()
  (flymake-mode 1)
  (when (fboundp 'flymake-systemd)
    (add-hook 'flymake-diagnostic-functions #'flymake-systemd nil t)))

(add-hook 'systemd-mode-hook #'+systemd-mode--setup)

;;;; pdf

(autoload 'pdf-view-mode "pdf-tools" nil t)

(alist-setq! auto-mode-alist "\\.pdf\\'" #'pdf-view-mode)
(alist-setq! magic-mode-alist "%PDF" #'pdf-view-mode)

(after-load! (:or pdf-tool pdf-view)
  (save-current-buffer
    (let ((val (pdf-tools-install :no-query))
          finished)
      (when (bufferp val)
        (with-current-buffer val
          (add-hook 'compilation-finish-functions
                    (lambda (buf status)
                      (setq finished t)))
          (while (not finished)
            (redisplay)
            (accept-process-output nil 1))
          (quit-windows-on val))))))

;;;; outline

(defun +outline-minor-faces ()
  "Enable `outline-minor-faces-mode' if not in a `help-mode' buffer."
  ;; outline-minor-faces-mode conflicts with \\[describe-mode].
  (unless (derived-mode-p 'help-mode)
    (outline-minor-faces-mode)))
(add-hook 'outline-minor-mode-hook '+outline-minor-faces)

;;;; visual-fill-column

(defun +visual-fill-column/toggle-visual-fill-and-center ()
  (interactive)
  (visual-fill-column-mode 'toggle)
  (setq-local visual-fill-column-center-text (symbol-value 'visual-fill-column-mode)))

;;;; hl-todo

(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'conf-mode-hook #'hl-todo-mode)

(after-load! hl-todo
  (define-keymap :keymap hl-todo-mode-map
    "M-g C-t [" #'hl-todo-previous
    "M-g C-t ]" #'hl-todo-next
    "M-g C-t o" #'hl-todo-occur
    "M-g C-t i" #'hl-todo-insert)
  (defvar-keymap +hl-todo-repeat-map
    "[" #'hl-todo-previous
    "]" #'hl-todo-next
    "o" #'hl-todo-occur)
  (put #'hl-todo-previous 'repeat-map +hl-todo-repeat-map)
  (put #'hl-todo-previous 'repeat-map +hl-todo-repeat-map))

;;;; display-line-numbers

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(setopt display-line-numbers-type 'relative)

;;;; display-fill-column-indicator-mode

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;; eglot

(declare-function eglot-completion-at-point "eglot.el")
(defun +eglot--capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'eglot-completion-at-point
                                     #'tempel-expand
                                     #'cape-file))))
(add-hook 'eglot-managed-mode-hook #'+eglot--capf)

(when (fboundp 'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(setopt eglot-autoshutdown t
        eglot-extend-to-xref t)

(defvar-keymap +eglot-prefix-map
  :prefix '+eglot-prefix-map
  "d" #'eglot-find-declaration
  "t" #'eglot-find-typeDefinition
  "i" #'eglot-find-implementation
  "s" #'consult-eglot-symbols
  "a" #'eglot-code-actions
  "n" #'eglot-code-action-inline
  "e" #'eglot-code-action-extract
  "r" #'eglot-code-action-rewrite
  "f" #'eglot-code-action-quickfix
  "o" #'eglot-code-action-organize-imports
  "/" #'eglot-format
  "c" #'eglot-show-call-hierarchy
  "T" #'eglot-show-type-hierarchy
  "w" #'eglot-show-workspace-configuration
  "C" #'eglot-signal-didChangeConfiguration
  "u" #'eglot-shutdown
  "U" #'eglot-shutdown-all)

(after-load! eglot
  (keymap-set eglot-mode-map "C-x x /" #'eglot-format)
  (keymap-set eglot-mode-map "C-c C-a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c l" '+eglot-prefix-map)
  (eglot-tempel-mode))

;;;;; consult-eglot

(after-load! consult
  (add-to-list 'consult-async-split-styles-alist `(space :separator ?\s :function ,#'consult--split-separator)))

(keymap-set search-map "s" #'consult-eglot-symbols)
(keymap-set search-map "M-s" #'consult-eglot-symbols)

(after-load! eglot
  (keymap-set eglot-mode-map "<remap> <xref-find-apropos>" 'consult-eglot-symbols))

(defun +consult--async-wrap--split-space (async)
  (consult--async-pipeline
   (consult--async-split 'space)
   async
   (consult--async-indicator)
   (consult--async-refresh)))

(after-load! (:and consult-eglot consult)
  (with-no-compile!
   (consult-customize consult-eglot-symbols
                      :async-wrap #'+consult--async-wrap--split-space)))

(define-advice consult-eglot-symbols (:around (fun) highlight)
  (cl-letf* ((orig-highlight (symbol-function 'consult--async-highlight))
             ((symbol-function 'consult--async-highlight)
              (lambda (&optional highlight)
                (funcall orig-highlight
                         (or highlight
                             (lambda (input)
                               (let ((re (rx-to-string (orderless-flex input) t)))
                                 (lambda (str)
                                   (consult--highlight-regexps (list re) t str)))))))))
    (funcall fun)))

;;;; xref

;; Use Ctrl and mouse click to jump to definitions, and Ctrl+Alt+mouse
;; click to jump to references.
(keymap-global-unset "C-<down-mouse-1>")
(keymap-global-unset "C-M-<down-mouse-1>")
(keymap-global-set "C-<mouse-1>" #'embark-dwim)
(keymap-global-set "C-M-<mouse-1>" #'xref-find-references-at-mouse)

(defvar +xref--max-definitions-in-buffer 5)
(defvar xref-buffer-name)

(defun +xref-window-quit ()
  (when (bufferp xref-buffer-name)
    (quit-windows-on xref-buffer-name)))

(declare-function xref-show-definitions-buffer-at-bottom "xref.el")

(defun +xref--show-definition (fetcher alist)
  "Use `xref-show-definitions-buffer' if the candidates are few.
Otherwise use `consult-xref'.

See `xref-show-xrefs' for FETCHER and ALIST."
  (let ((xrefs (funcall fetcher)))      ; retrieve candidate xrefs list
    (cond ((length= xrefs 1)            ; exactly one result: jump directly
           (+xref-window-quit)          ; close any temporary xref window
           (xref-show-definitions-buffer-at-bottom fetcher alist))
          ((length< xrefs +xref--max-definitions-in-buffer)
                                        ; few results: use a small dedicated buffer
           (xref-show-definitions-buffer-at-bottom fetcher alist))
          (t                            ; many results: let consult show them
           (+xref-window-quit)
           (consult-xref fetcher alist)))))

(after-load! xref
  (setq xref-search-program
        (cond ((executable-find "rg") 'ripgrep)
              ((executable-find "ugrep") 'ugrep)
              (t 'grep))
        xref-show-definitions-function #'+xref--show-definition
        xref-auto-jump-to-first-definition t)
  (remove-hook 'xref-after-jump-hook #'recenter)
  (add-hook 'xref-after-jump-hook #'reposition-window))

;;;; good-doc

(autoload 'good-doc-lookup "good-doc" nil t)

;;;; rust-docs

(autoload 'rust-docs-lookup "rust-docs" nil t)

;;;; javascript

(declare-function js-jsx--comment-region "js.el")

(define-advice js-jsx-enable (:after () comments)
  "Enable JSX comments."
  (setq-local comment-region-function #'js-jsx--comment-region))

(define-advice js-jsx-enable (:after () sgml)
  "Enable sgml commands in JSX buffers."
  (eval-and-compile (require 'sgml-mode))
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map)
    (define-key map (kbd "C-c C-b") #'sgml-skip-tag-backward)
    (define-key map (kbd "C-c C-d") #'sgml-delete-tag)
    (define-key map (kbd "C-c C-e") #'sgml-close-tag)
    (define-key map (kbd "C-c C-f") #'sgml-skip-tag-forward)
    (define-key map (kbd "C-c C-o") #'sgml-tag)
    (define-key map (kbd "C-c C-t") #'sgml-tag)))

;;;; JSON

(defun copy-json-as-lisp (beg end)
  "Convert JSON between BEG–END to Lisp data and copy to kill ring.
With no active region, operate on the whole buffer."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((str (buffer-substring-no-properties beg end))
        (pp-default-function 'pp-29))
    (if (string-blank-p str)
        (user-error "Empty string")
      (kill-new
       (pp-to-string
        (json-parse-string str :object-type 'alist))))))

;;;; paren-face

(add-hook 'lisp-data-mode-hook #'paren-face-mode)
(add-hook 'scheme-mode-hook #'paren-face-mode)

;;;; paredit

(add-hook 'lisp-data-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)

;;;; elec-pair

(after-load! elec-pair
  (keymap-set electric-pair-mode-map "]" #'up-list))

;;;; lisp

(after-load! sly
  (keymap-set sly-prefix-map "M-h" #'sly-documentation-lookup))

;;;; elisp-mode

(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(after-load! elisp-mode
  (when (boundp 'trusted-content)
    (add-to-list 'trusted-content (locate-user-emacs-file "site-lisp/"))
    (add-to-list 'trusted-content (abbreviate-file-name (straight--build-dir)))
    (add-to-list 'trusted-content (abbreviate-file-name (straight--repos-dir))))
  (when (native-comp-available-p)
    (keymap-set emacs-lisp-mode-map "C-c C-l" #'emacs-lisp-native-compile-and-load))
  (keymap-set lisp-interaction-mode-map "C-c C-j" #'eval-print-last-sexp))

(defun straight-flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for elisp byte compilation.
Spawn an Emacs process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
  (unless (trusted-content-p)
    ;; FIXME: Use `bwrap' and friends to compile untrusted content.
    ;; FIXME: We emit a message *and* signal an error, because by default
    ;; Flymake doesn't display the warning it puts into "*flmake log*".
    (message "Disabling straight-flymake-byte-compile in %s (untrusted content)"
             (buffer-name))
    (user-error "Disabling straight-flymake-byte-compile in %s (untrusted content)"
                (buffer-name)))
  (when elisp-flymake--byte-compile-process
    (when (process-live-p elisp-flymake--byte-compile-process)
      (kill-process elisp-flymake--byte-compile-process)))
  (let ((temp-file (make-temp-file "straight-flymake-byte-compile"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer (generate-new-buffer " *straight-flymake-byte-compile*"))
           ;; Hack: suppress warning about missing lexical cookie in
           ;; *scratch* buffers.
           (warning-suppression-opt
            (and (derived-mode-p 'lisp-interaction-mode)
                 '("--eval"
                   "(setq bytecomp--inhibit-lexical-cookie-warning t)"))))
      (setq-local
       elisp-flymake--byte-compile-process
       (make-process
        :name "straight-flymake-byte-compile"
        :buffer output-buffer
        :command `(,(expand-file-name invocation-name invocation-directory)
                   "-Q"
                   "--batch"
                   "--eval"
                   ,(prin1-to-string
                     `(let ((recipes ',(map-apply (lambda (p r)
                                                    (cons (intern p) r))
                                                  straight--recipe-cache)))
                        (setopt straight-use-symlinks ,straight-use-symlinks
                                straight-build-dir ,straight-build-dir)
                        (load ,(expand-file-name
                                "straight/repos/straight.el/bootstrap.el"
                                (or (bound-and-true-p straight-base-dir)
                                    user-emacs-directory)))
                        (dolist (recipe recipes)
                          (straight-register-package recipe))
                        (dolist (recipe recipes)
                          (straight-use-package recipe))))
                   ;; "--eval" "(setq load-prefer-newer t)" ; for testing
                   ,@(mapcan (lambda (path) (list "-L" path))
                             elisp-flymake-byte-compile-load-path)
                   ,@warning-suppression-opt
                   "-f" "elisp-flymake--batch-compile-for-flymake"
                   ,temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond
                 ((not (and (buffer-live-p source-buffer)
                            (eq proc (with-current-buffer source-buffer
                                       elisp-flymake--byte-compile-process))))
                  (flymake-log :warning
                               "byte-compile process %s obsolete" proc))
                 ((zerop (process-exit-status proc))
                  (elisp-flymake--byte-compile-done report-fn
                                                    source-buffer
                                                    output-buffer))
                 (t
                  (funcall report-fn
                           :panic
                           :explanation
                           (format "byte-compile process %s died " proc))))
              (ignore-errors (delete-file temp-file))
              ;; (with-current-buffer output-buffer
              ;;   (message "Log: %s" (buffer-string)))
              (kill-buffer output-buffer))))
        :stderr " *stderr of straight-flymake-byte-compile*"
        :noquery t)))))

(advice-add 'elisp-flymake-byte-compile :override #'straight-flymake-byte-compile)

;;;; pp

(keymap-global-set "M-:" #'pp-eval-expression)

;;;; pp-posframe

;; Show results of `eval-last-sexp' in posframes.

(autoload 'pp-posframe-eval-last-sexp "pp-posframe"
  "Evaluate sexp before point; display the value in a posframe." t)
(autoload 'pp-posframe-compile-defun "pp-posframe"
  "Compile and evaluate the current top-level form.
Display the result in a posframe." t)
(autoload 'pp-posframe-macroexpand-last-sexp "pp-posframe"
  "Macroexpand the sexp before point; display the result in a posframe." t)
(keymap-global-set "C-x C-e" #'pp-posframe-eval-last-sexp)
(after-load! elisp-mode
  (keymap-set emacs-lisp-mode-map "C-M-x" #'pp-posframe-compile-defun)
  (keymap-set emacs-lisp-mode-map "C-c M-e" #'pp-posframe-macroexpand-last-sexp))

;;;; eldoc

(after-load! eldoc
  (eldoc-add-command                    ; for eldoc-diffstat
   'magit-next-line 'magit-previous-line
   'magit-section-forward 'magit-section-backward
   'magit-section-forward-sibling 'magit-section-backward-sibling
   'magit-blame-next-chunk 'magit-blame-previous-chunk))

;;;; cc-mode

(setq c-tab-always-indent nil
      c-insert-tab-function #'completion-at-point)
(autoload 'flymake-clang-tidy "flymake-clang-tidy")
(defun +cc-mode--hook ()
  (add-hook 'flymake-diagnostic-functions #'flymake-clang-tidy nil t))
(add-hook 'c-mode-common-hook '+cc-mode--hook)

(alist-setq! auto-mode-alist
  "\\.[cti]pp\\'" #'c++-mode)

;;;; rust-mode

(declare-function project-prefixed-buffer-name "project.el" (arg1))
(define-advice rust--compile (:around (&rest args) project-prefix-buffer-name)
  (let ((compilation-buffer-name-function #'project-prefixed-buffer-name))
    (apply args)))

;;;; ruby

;;;; sh-script

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(autoload 'flymake-checkbashisms "flymake-checkbashisms")
(defun +sh-mode-h ()
  (add-hook 'flymake-diagnostic-functions #'flymake-checkbashisms nil t))
(after-load! 'sh-script
  (add-hook 'sh-mode-hook #'+sh-mode-h))

(defun +shellcheck-auto-fix ()
  "Run shellcheck on current buffer to generate fix suggestions.
Displays suggested fixes as a diff in a temporary buffer. Prompts
user to apply the changes interactively. Requires shellcheck
executable in PATH. Applies selected changes immediately when
confirmed."
  (interactive)
  (with-output-to-temp-buffer " *shellcheck diff*"
    (call-process-region nil nil "shellcheck" nil standard-output t "-f" "diff" "-"))
  (let ((name (or (and buffer-file-name
                       (file-name-nondirectory
                        (buffer-file-name)))
                  (buffer-name))))
    (with-current-buffer " *shellcheck diff*"
      (let ((inhibit-read-only t)
            (buffer-read-only nil))
        (replace-regexp-in-region "^\\(+++ b\\|--- a\\)/-"
                                  (concat "\\1/"
                                          (replace-quote name)))
        (diff-mode)
        (font-lock-ensure))))
  (when (or noninteractive (yes-or-no-p "Apply the patch? "))
    (with-current-buffer " *shellcheck diff*"
      (diff-apply-buffer)))
  (quit-windows-on " *shellcheck diff*" t))

;;;; udev rules

;; Use prog-mode to edit udev rules.

(defun udev-rules-mode ()
  "Turn on `prog-mode' mode and set up for udev rules."
  (interactive)
  (prog-mode)
  (let ((st (syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)))

(alist-setq! auto-mode-alist
  "\\.rules\\'" #'udev-rules-mode)

;;;; project

(setq project-list-file (locate-user-emacs-file '("projects.eld.zst")))

(declare-function project-root "project.el" (&rest rest))
(defun +project--external-roots ()
  (and-let* ((project (project-current))
             (root (project-root project))
             (project-root-file (expand-file-name ".project-root" root))
             ((file-exists-p project-root-file)))
    (with-temp-buffer
     (insert-file-contents project-root-file)
     (let ((default-directory root))
       (mapcar #'expand-file-name
               (string-lines (buffer-string) t nil))))))

(after-load! project
  (when (commandp 'project-prefix-or-any-command)
    (setopt project-switch-commands 'project-prefix-or-any-command))

  (setq-default project-vc-external-roots-function #'+project--external-roots)
  (setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)

  (setopt project-vc-ignores (seq-union project-vc-ignores '(".pc")))
  (setopt project-vc-extra-root-markers
          (seq-union project-vc-extra-root-markers
                     '(".project-root" "configure.ac" "Cargo.toml" "package.json")))
  (when (and (not (functionp project-switch-commands))
             (consp project-switch-commands))
    (add-to-list 'project-switch-commands '(project-compile "Compile") t)))

;;;; buffer-env

(add-hook 'hack-local-variables-hook #'buffer-env-update)
(add-hook 'comint-mode-hook #'buffer-env-update)
(defvar buffer-env--cache)
(defun +buffer-env/clear-cache ()
  "Clear buffer-env cache."
  (interactive)
  (clrhash buffer-env--cache))
(defvar buffer-env-command-alist)
(after-load! buffer-env
  (alist-setq! buffer-env-command-alist "/\\.nvmrc\\'" "~/.nvm/nvm-exec env -0")
  (setopt buffer-env-script-name '(".envrc" ".nvmrc" ".env")))

;;;; tramp

(setq tramp-persistency-file-name
      (locate-user-emacs-file "tramp.eld"))

;;; We need to ensure Unix domain sockets have paths shorter than 108 characters
;;; (this is a system limit). If tramp-compat-temporary-file-directory is too long,
;;; we'll use a shorter alternative location.
(defvar tramp-compat-temporary-file-directory)
(after-load! tramp-compat
  ;; Check if the current directory path is longer than 50 characters.
  ;; This provides a safety margin since socket names will be appended to this path.
  (when (length> tramp-compat-temporary-file-directory 50)
    ;; Set a shorter alternative directory in XDG_RUNTIME_DIR
    (setq tramp-compat-temporary-file-directory
          (substitute-in-file-name "$XDG_RUNTIME_DIR/emacs"))
    ;; Create the directory if it doesn't exist yet
    ;; The 't' parameter creates parent directories as needed
    (unless (file-directory-p tramp-compat-temporary-file-directory)
      (mkdir tramp-compat-temporary-file-directory t))
    (chmod tramp-compat-temporary-file-directory #o700)))

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

;;;; vc

(setq vc-follow-symlinks t)
(setq vc-svn-global-switches
      '("--config-option"
        "config:auth:password-stores=gnome-keyring")
      vc-svn-diff-switches '("-x" "-u -p"))
(keymap-set vc-prefix-map "." '+vc/dir-here)

(defun +vc/dir-here (&optional backend)
  (interactive
   (list
    (and current-prefix-arg
         (vc-read-backend "Prompt: "))))
  (vc-dir default-directory backend))

(defun +project/vc-diff ()
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (vc-backend (or (and (eq (car-safe project) 'vc)
                              (cadr project))
                         (vc-responsible-backend root "no-error")
                         (vc-read-backend "Backend: ")))
         (default-directory root))
    (vc-diff nil nil (list vc-backend (list root)))))
(keymap-set project-prefix-map "=" #'+project/vc-diff)

;;;; magit

(after-load! project
  (when (consp project-switch-commands)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))
(defvar magit-credential-cache-daemon-process)
(defun +magit--ccdp-no-query ()
  "Avoid query process status on exit."
  (when (bound-and-true-p magit-credential-cache-daemon-process)
    (set-process-query-on-exit-flag
     magit-credential-cache-daemon-process nil)))
(after-load! magit
  (transient-set-default-level 'magit:--gpg-sign 1)
  (transient-set-default-level 'magit:--signoff 1)
  (setopt magit-openpgp-default-signing-key "ABE50B31E2F0C94AC4585BC78D97BF3F6BFA0BDA")
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)
  (advice-add #'magit-maybe-start-credential-cache-daemon :after '+magit--ccdp-no-query)
  (setopt magit-wip-mode-lighter "")
  (magit-wip-mode))

(setq git-commit-major-mode #'log-edit-mode)

(defun +git-commit--log-edit-h ()
  (let ((params
         `((log-edit-listfun . ,#'magit-staged-files)
           (log-edit-diff-function . ,#'magit-diff-while-committing))))
    (dolist (crt params)
      (set (make-local-variable (car crt)) (cdr crt)))
    (run-hooks 'log-edit-hook)
    (save-buffer)))

(after-load! git-commit
  (add-hook 'git-commit-post-finish-hook #'log-edit-hide-buf)
  (add-hook 'git-commit-setup-hook #'+git-commit--log-edit-h 90))

;;;; forge

(after-load! magit
  (require 'forge))

;;;; Add-Log

(defvar vc-dwim-post-commit-hook nil)

(defun add-log/vc-dwim-commit ()
  "Invoke vc-dwim --commit with the current file.

Run hook `vc-dwim-post-commit-hook'."
  (interactive)
  (let ((default-directory (vc-root-dir))
        exitcode)
    (with-output-to-temp-buffer "*vc-dwim*"
      (setq exitcode
            (process-file "vc-dwim" nil standard-output nil
                          "--commit"
                          (file-relative-name (buffer-file-name))))
      (with-current-buffer "*vc-dwim*"
        (delay-mode-hooks
          (diff-mode))
        (font-lock-ensure)
        (unless (zerop exitcode)
          (goto-char (point-min))
          (user-error "%s" (buffer-substring-no-properties (point) (line-end-position)))
          (add-face-text-property (point) (line-end-position) 'error 'append))))
    (when (zerop exitcode)
      (run-hooks 'vc-dwim-post-commit-hook))))

(defvar add-log-always-start-new-record)
(defvar add-log-buffer-file-name-function)

(define-advice add-change-log-entry
    (:around (fun whoami changelog-file-name &rest rest) always-start-new-record)
  "Ensure a new change log entry is always started if file is unmodified.

This advice ensures that a new change log entry is always started if the
current change log file has not been modified. This behavior is useful
for automating the creation of change log entries, especially when
running commands in bulk or using version control systems that expect a
new entry for each change.

If the change log file is managed by version control and up-to-date, or
if no version control system is being used, this advice will set
`add-log-always-start-new-record' to t, thereby forcing the creation of
a new change log entry.

The main logic involves checking the modification status of the current
change log buffer and determining whether the change log file is under
version control. If the file is up-to-date or not under version control,
it sets the `add-log-always-start-new-record' variable to t to ensure a
new record is started."
  (interactive (list current-prefix-arg
                     (prompt-for-change-log-name)))
  (let* ((add-log-always-start-new-record add-log-always-start-new-record)
         (buf-file-name (funcall add-log-buffer-file-name-function))
         (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
         (changelog-file-name (expand-file-name (find-change-log
                                                 changelog-file-name
                                                 buffer-file)))
         (changelog-buf (add-log-find-changelog-buffer changelog-file-name)))
    (unless (buffer-modified-p changelog-buf)
      (if (vc-registered changelog-file-name)
          (when (vc-up-to-date-p changelog-file-name)
            (setq add-log-always-start-new-record t))
        (setq add-log-always-start-new-record t)))
    (apply fun whoami changelog-file-name rest)))

(after-load! add-log
  (keymap-set change-log-mode-map "C-c RET" #'add-log/vc-dwim-commit))

(after-load! vc
  (add-hook 'vc-dwim-post-commit-hook #'vc-refresh-state))
(after-load! magit
  (add-hook 'vc-dwim-post-commit-hook #'magit-refresh))
(after-load! diff-hl
  (add-hook 'vc-dwim-post-commit-hook #'diff-hl-update))

;;;; diff-hl

(autoload 'diff-hl-magit-post-refresh "diff-hl")
(add-hook 'tty-setup-hook #'diff-hl-margin-mode)
(after-load! magit-mode
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(setq diff-hl-margin-symbols-alist '((insert  . "增")
                                     (delete  . "刪")
                                     (change  . "改")
                                     (unknown . "疑")
                                     (ignored . "略")))
(setq diff-hl-update-async t)

(define-advice diff-hl-margin-ensure-visible (:override () auto-width)
  "Ensure that diff-hl margin is wide enough to display all symbols.
Calculate the maximum width of all symbols in `diff-hl-margin-symbols-alist'
and set the appropriate margin width variable accordingly.
Then refresh all windows displaying the current buffer."
  (require 'map)
  (let ((width-var (intern (format "%S-margin-width" diff-hl-side))))
    (set width-var (apply #'max (map-values-apply #'string-width diff-hl-margin-symbols-alist))))
  (dolist (win (get-buffer-window-list))
    (set-window-buffer win (current-buffer))))
(add-hook 'diff-hl-margin-mode-hook #'diff-hl-margin-ensure-visible)

(after-load! (:or diff-hl vc magit)
  (global-diff-hl-mode)
  (keymap-set diff-hl-mode-map "C-c v" diff-hl-command-map))

;;;; eldoc-diffstat

(after-load! magit-mode
  (add-hook 'magit-mode-hook #'eldoc-diffstat-mode)
  (add-hook 'magit-blame-mode-hook #'eldoc-diffstat-mode))
(add-hook 'vc-annotate-mode-hook #'eldoc-diffstat-mode)

;;;; eat

(after-load! eat
  (setopt eat-semi-char-non-bound-keys
          (seq-union '([?\e ?o])
                     eat-semi-char-non-bound-keys)))

(unless (memq system-type '(ms-dos windows-nt))
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(after-load! project
  (keymap-set project-prefix-map "t" #'eat-project)
  (keymap-set project-other-window-map "t" #'eat-project-other-window)
  (when (consp project-switch-commands)
    (add-to-list 'project-switch-commands '(eat-project "Eat") t)))

(defvar eat-terminal)
(declare-function eat-term-send-string "eat.el" (terminal string))
(declare-function eat-self-input "eat.el" (n &optional e))

(setopt eat-kill-buffer-on-exit t)

(after-load! eat
  (alist-setq! eat-tramp-shells
    "sshx" "/bin/bash"
    "ssh" "/bin/bash"
    "scp" "/bin/bash"
    "scpx" "/bin/bash" ))

(defun +eat-install-helpers ()
  (interactive)
  (unless (file-remote-p default-directory)
    (user-error "Not in a remote directory"))
  (copy-directory eat-term-terminfo-directory
                  (concat (file-remote-p default-directory) "~/.terminfo")
                  nil t t)
  (copy-directory eat-term-shell-integration-directory
                  (concat (file-remote-p default-directory) "~/.cache/eat-shell-integration")
                  nil t t))

;;;; with-editor

(keymap-global-set "<remap> <async-shell-command>" #'with-editor-async-shell-command)
(keymap-global-set "<remap> <shell-command>" #'with-editor-shell-command)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'term-exec-hook #'with-editor-export-editor)
(add-hook 'vterm-mode-hook #'with-editor-export-editor)

;;;; pyim

(declare-function pyim-cregexp-build "pyim-cregexp.el")
(defun +orderless-pinyin (component)
  (require 'pyim)
  (pyim-cregexp-build component 3 t))

(defvar +pyim--corfu nil)
(defun +pyim--activate ()
  (when (boundp 'corfu-auto)
    (set (make-local-variable '+pyim--corfu) corfu-auto)
    (set (make-local-variable 'corfu-auto) nil)))
(defun +pyim--deactivate ()
  (if (boundp 'corfu-auto)
      (set (make-local-variable 'corfu-auto)
           +pyim--corfu)))
(after-load! corfu
  (add-hook 'pyim-activate-hook '+pyim--activate)
  (add-hook 'pyim-deactivate-hook '+pyim--deactivate))

(after-load! orderless
  ;; 通过拼音搜索中文
  (alist-setq! orderless-affix-dispatch-alist ?` #'+orderless-pinyin))

(after-load! pyim
  (pyim-basedict-enable))

;;;; rime

(after-load! (:and rime meow)
  (setopt rime-disable-predicates '(meow-normal-mode-p
                                    meow-keypad-mode-p
                                    meow-motion-mode-p
                                    meow-beacon-mode-p)))
(define-advice toggle-input-method (:before (&rest _) rime)
  (setq default-input-method "rime"))

;;;; kinsoku

(setq word-wrap-by-category t)

;;;; kkp

(when (or (daemonp) (eq t (terminal-live-p nil)))
  (global-kkp-mode))

;;;; xterm

(setopt xterm-set-window-title t)

;;;; xt-mouse

(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(after-load! xt-mouse
  (xterm-mouse-mode))
(if (eq (framep-on-display) t)
    (require 'xt-mouse))

;;;; clipetty

(add-hook 'tty-setup-hook #'clipetty-mode)
(after-load! clipetty
  (global-clipetty-mode))
(if (eq (framep-on-display) t) (require 'clipetty))

;;;; Display Table

;; Set up display table to use unicode chars to display frame borders
;; in terminal

(add-hook 'emacs-startup-hook #'standard-display-unicode-special-glyphs)

;;;; repeat

(defun +repeat--post-command ()
  (when (and (symbolp this-command) (function-get this-command 'repeat-map))
    (message "Command %S has a `repeat-map'" this-command)
    (require 'repeat)
    (declare-function repeat-post-hook "repeat.el")
    (repeat-post-hook)))
(add-hook 'post-command-hook '+repeat--post-command)
(after-load! repeat
  (remove-hook 'post-command-hook '+repeat--post-command)
  (repeat-mode))

;;;; savehist

(setq savehist-file (locate-user-emacs-file '("savehist.eld.zst")))
(setq savehist-additional-variables '(kill-ring
                                      register-alist
                                      compile-command
                                      corfu-history))
(add-hook 'after-init-hook #'savehist-mode)

;;;; auth-sources
(setopt auth-sources '("~/.authinfo.gpg"))
(after-load! auth-sources
  (setopt auth-source-save-behavior t
          auth-source-gpg-encrypt-to (list  "0xBBE2757FC7BFC23B"))
  (auth-source-forget-all-cached))

;;;; lin

(defvar lin-mode-hooks)
(setq lin-mode-hooks
      '( gnus-group-mode-hook gnus-server-mode-hook
         gnus-summary-mode-hook mu4e-main-mode-hook magit-mode-hook
         backup-list-mode-hook deadgrep-mode-hook rg-mode-hook
         archive-mode-hook bongo-mode-hook dired-mode-hook
         elfeed-search-mode-hook git-rebase-mode-hook grep-mode-hook
         ibuffer-mode-hook ilist-mode-hook ledger-report-mode-hook
         log-view-mode-hook magit-log-mode-hook mu4e-headers-mode-hook
         notmuch-search-mode-hook notmuch-tree-mode-hook
         occur-mode-hook org-agenda-mode-hook
         pdf-outline-buffer-mode-hook proced-mode-hook
         tabulated-list-mode-hook tar-mode-hook world-clock-mode-hook
         telega-root-mode-hook))
(dolist (hook lin-mode-hooks)
  (add-hook hook #'lin-mode))

(after-load! lin
  (setopt lin-face 'lin-magenta)
  (setopt lin-mode-hooks
          (seq-union lin-mode-hooks (custom--standard-value 'lin-mode-hooks)))
  (lin-global-mode))

;; Highlight current line in the error buffer after running `next-error'.
(declare-function hl-line-highlight "hl-line.el")
(defun +lin-line--next-error-h ()
  "Highlight the current line in the error buffer."
  (save-selected-window
    (when-let* ((win (get-buffer-window next-error-buffer)))
      (select-window win)
      (recenter))
    (when (bound-and-true-p lin-mode)
      (hl-line-highlight))))
(add-hook 'next-error-hook '+lin-line--next-error-h)

;; Highlight the current gnus header buffer item.
(after-load! gnus-sum
  (add-hook 'gnus-visual-mark-article-hook #'hl-line-highlight))

;;;; prism

(defun +prism--set-colors ()
  "Configure colors used by prism according to the current theme.
This function sets up a palette of 16 colors with varying levels
of saturation and lightness. For known themes (Modus and EF),
it uses theme-specific colors. Otherwise, it blends generic colors
with the default foreground. Special handling is also provided for
comments and strings."
  (prism-set-colors
   :num 16
   ;; Create a series of desaturations from 0 to 37.5 in steps of 2.5
   :desaturations (cl-loop for i from 0 below 16
                           collect (* i 2.5))
   ;; Create a series of lightness values from 0 to 37.5 in steps of 2.5
   :lightens (cl-loop for i from 0 below 16
                      collect (* i 2.5))
   ;; Choose colors based on the active theme
   :colors (cond
            ;; For Modus themes, use theme-specific colors
            ((and (fboundp 'modus-themes--list-enabled-themes)
                  (modus-themes--list-enabled-themes))
             (with-no-compile!
              (modus-themes-with-colors
                (list pink fg-alt green indigo))))

            ;; For EF themes, use theme-specific colors
            ((and (fboundp 'ef-themes--list-enabled-themes)
                  (ef-themes--list-enabled-themes))
             (with-no-compile!
              (ef-themes-with-colors
               (list red green magenta cyan))))

            ;; For other themes, blend generic colors with foreground
            (t
             (let ((foreground (face-attribute 'default :foreground)))
               (mapcar (lambda (c) (prism-blend c foreground 0.5))
                       (list "pink" "green" "magenta" "cyan")))))

   ;; Custom function for comments - blend with comment face color
   :comments-fn
   (lambda (color)
     (prism-blend color
                  (face-attribute 'font-lock-comment-face :foreground) 0.25))

   ;; Custom function for strings - blend with default foreground
   :strings-fn
   (lambda (color)
     (prism-blend color (face-attribute 'default :foreground) 0.5))))

(defun +prism--enable-theme-f (_theme)
  "Refresh prism colors when a theme is enabled.
This is added to `enable-theme-functions'. The _THEME argument is
not used, but is required by the hook."
  (run-with-timer 0 nil #'+prism--set-colors))

(after-load! prism
  ;; Set initial colors when prism is loaded
  (+prism--set-colors)
  ;; Make sure prism colors are updated when themes change
  (add-hook 'enable-theme-functions #'+prism--enable-theme-f 100))

;;;; email and gnus

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(after-load! gnus-art
  (require 'gnus-diff))

;;;; elfeed

(setq elfeed-search-remain-on-entry t)
(setq elfeed-show-entry-switch #'+elfeed-display-buffer)

(defun +elfeed-display-buffer (buf)
  (pop-to-buffer buf '((display-buffer-at-bottom)))
  (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

(defun +elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
  (lambda (times)
    (interactive "p")
    (forward-line (* times (or lines 0)))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1))))

(defun +elfeed-update-search-buffer (&rest _)
  (when-let* ((buf (elfeed-search-buffer))
              (window (get-buffer-window buf nil)))
    (with-selected-window window
      (hl-line-highlight))))

(advice-add #'elfeed-show-next :after #'+elfeed-update-search-buffer)
(advice-add #'elfeed-show-prev :after #'+elfeed-update-search-buffer)

(setq elfeed-show-entry-delete
      (lambda ()
        (when (derived-mode-p 'elfeed-show-mode)
          (elfeed-kill-buffer))))

(defvar +elfeed-tag-history nil
  "History variable for tags used in `elfeed-show-tag'.")

(define-advice elfeed-show-tag (:before (&rest args) completing-read)
  "Replace default prompting with a completing-read interface for tags.
This advice enhances `elfeed-show-tag' to use `completing-read-multiple'
for selecting multiple tags from available tags in the database, with
history support."
  (interactive
   (mapcar #'intern
           (completing-read-multiple
            "Tags: "
            (elfeed-db-get-all-tags)
            nil nil nil '+elfeed-tag-history)))
  args)

(defun +elfeed-browse-eww ()
  "Open the current Elfeed entry in EWW browser.
This temporarily sets `browse-url-browser-function' to use EWW
and then calls `elfeed-show-visit' to open the entry URL."
  (interactive)
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit)))

(defvar +elfeed-search-live-filter-history nil)

(defun +elfeed-search-live-filter-with-history ()
  (interactive)
  (unwind-protect
      (let ((elfeed-search-filter-active :live))
        (setq elfeed-search-filter
              (read-from-minibuffer "Filter: " elfeed-search-filter
                                    nil nil '+elfeed-search-live-filter-history)))
    (elfeed-search-update :force)))

(defun +elfeed-scroll-up-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-up-command arg)
      (error (elfeed-show-next)))))

(defun +elfeed-scroll-down-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-down-command arg)
      (error (elfeed-show-prev)))))

(after-load! elfeed
  (keymap-set elfeed-search-mode-map "q" #'elfeed-db-unload)
  (keymap-set elfeed-search-mode-map "n" (+elfeed-search-show-entry-pre +1))
  (keymap-set elfeed-search-mode-map "p" (+elfeed-search-show-entry-pre -1))
  (keymap-set elfeed-search-mode-map "M-RET" (+elfeed-search-show-entry-pre))
  (keymap-substitute elfeed-search-mode-map
                     #'elfeed-search-live-filter
                     #'+elfeed-search-live-filter-with-history)
  (keymap-set elfeed-show-mode-map "e" #'+elfeed-browse-eww)
  (keymap-set elfeed-show-mode-map "SPC" '+elfeed-scroll-up-command)
  (keymap-set elfeed-show-mode-map "S-SPC" '+elfeed-scroll-down-command))

(defvar +feeds-file-watch-descriptor nil)

(defun +elfeed-load-feeds ()
  "Load feeds from the feeds.eld file in `elfeed-db-directory'.
If the feeds.eld file exists, it will be loaded and its contents
will be set as `elfeed-feeds'. This allows for dynamic loading
of feed configurations without modifying init files."
  (let ((default-directory elfeed-db-directory))
    (when (file-exists-p "feeds.eld")
      (with-temp-buffer
        (insert-file-contents "feeds.eld")
        (setq elfeed-feeds (read (current-buffer)))
        (message "Updated elfeed feeds from %s"
                 (expand-file-name "feeds.eld" elfeed-db-directory))))))

(after-load! elfeed-db
  ;; Load feeds from the feeds.eld file when elfeed-db is loaded
  (+elfeed-load-feeds)

  ;; Set up a file watcher on the feeds.eld file if we haven't already
  (when (null +feeds-file-watch-descriptor)
    ;; Make sure we have the filenotify library available
    (require 'filenotify)

    ;; Create and store a file watcher that will reload feeds when the file changes
    (setq +feeds-file-watch-descriptor
          (file-notify-add-watch
           ;; Watch the feeds.eld file in the elfeed database directory
           (expand-file-name "feeds.eld" elfeed-db-directory)
           ;; Only watch for change events
           '(change)
           ;; Callback function that runs when a change is detected
           (pcase-lambda (`(,descriptor ,action . ,files))
             ;; Check the type of notification
             (pcase action
               ;; When the file has changed, reload the feeds
               ('changed
                (+elfeed-load-feeds))))))))

;;;; emacs-server

;; Workaround windows encoding issue
(defun +server--process-filter-coding-system (&rest args)
  (let ((file-name-coding-system locale-coding-system))
    (apply args)))

(after-load! server
  (require 'org-protocol)

  ;; If the operating system is either Windows (windows-nt) or DOS (ms-dos),
  ;; add an advice around the server process filter function to correctly handle coding systems.
  (when (memq system-type '(windows-nt ms-dos))
    (advice-add #'server-process-filter :around '+server--process-filter-coding-system)))

(defun +import-env-var-for-display ()
  (let ((display-name (or (frame-parameter nil 'display) x-display-name)))
    (pcase display-name
      ('nil nil)
      ((pred (string-match-p "\\`wayland-"))
       (setenv "WAYLAND_DISPLAY" display-name))
      ((pred (string-match-p "\\`:"))
       (setenv "DISPLAY" display-name)))))

(add-hook 'server-after-make-frame-hook #'+import-env-var-for-display)

(defun +niri-xdg-open (url &optional _ignored)
  (call-process "niri" nil 0 nil
                "msg" "action" "spawn" "--" "xdg-open" url))

(function-put '+niri-xdg-open 'browse-url-browser-kind 'external)

(when (getenv "NIRI_SOCKET")
  (setq browse-url-browser-function #'+niri-xdg-open))

;;;; epg

;; (setopt epg-pinentry-mode 'loopback)
(setopt epa-keys-select-method 'minibuffer)

;;;; tui

(autoload 'tui-run    "tui" nil t)
(autoload 'tui-rg     "tui" nil t)
(autoload 'tui-ugrep  "tui" nil t)
(autoload 'tui-yazi   "tui" nil t)
(autoload 'tui-kill   "tui" nil t)
(autoload 'tui-line   "tui" nil t)
(autoload 'tui-find   "tui" nil t)
(autoload 'tui-locate "tui" nil t)
(after-load! tui
  (alist-setq! display-buffer-alist "^\\*tui-" '((display-buffer-same-window))))

;;;; gptel

(setq gptel-default-mode #'markdown-mode)
(defun +gptel-mode-h ()
  (when (derived-mode-p 'org-mode)
    (setq-local org-hide-emphasis-markers t)))
(add-hook 'gptel-mode-hook #'+gptel-mode-h)

(after-load! gptel
  (require 'gptel-config)
  (keymap-set gptel-mode-map "C-j" #'gptel-send))

(after-load! gptel-aibo
  (keymap-set gptel-aibo-mode-map "C-j" #'gptel-aibo-send))

;;;; Claude Code IDE

(setq claude-code-ide-terminal-backend 'eat)
(setq claude-code-ide-cli-path
      (expand-file-name "scripts/claude" user-emacs-directory))

;;;; ECA

(after-load! consult
  (alist-setq! consult-mode-histories
    eca-chat-mode '(eca-chat--history eca-chat--history-index markdown-beginning-of-line)))

;;;; logos

(keymap-global-set "<f8>" #'logos-focus-mode)
(keymap-global-set "<remap> <narrow-to-region>" #'logos-narrow-dwim)
(keymap-global-set "<remap> <forward-page>" #'logos-forward-page-dwim)
(keymap-global-set "<remap> <backward-page>" #'logos-backward-page-dwim)

(defun logos-focus--narrow ()
  (declare-function logos--narrow-to-page "logos.el")
  (when (symbol-value 'logos-focus-mode)
    (logos--narrow-to-page 0)
    (make-local-variable 'logos--restore)
    (push #'widen logos--restore)))

(after-load! logos
  (define-keymap :keymap logos-focus-mode-map
    "<left>" #'logos-backward-page-dwim
    "<right>" #'logos-forward-page-dwim)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)"
                               logos-page-delimiter)))
        logos-outlines-are-pages t)
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  (add-hook 'logos-focus-mode-hook #'logos-focus--narrow)
  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers))

;;;; image-slicing

(autoload 'image-slicing-mode "image-slicing" nil t)
(add-hook 'eww-mode-hook #'image-slicing-mode)

;;;; bookmark

(defvar pp-default-function)
(defun +bookmark--pp-28 (&rest args)
  (let ((pp-default-function 'pp-28))
    (apply args)))

(autoload 'url-bookmark-add "bookmark-extras" "" t)
(keymap-global-set "C-x r u" #'url-bookmark-add)
(after-load! bookmark
  (advice-add #'bookmark-write-file :around '+bookmark--pp-28)

  (setq bookmark-save-flag 1
        bookmark-watch-bookmark-file 'silent
        bookmark-version-control t)

  (require 'bookmark-extras))

;;;; org

(autoload 'org-store-link "ol" nil t)

(defvar org-hide-emphasis-markers)

(defun +org/toggle-emphasis-markers ()
  "Toggle the display of emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-flush))

(after-load! org
  (setq org-modern-table nil)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (setq valign-fancy-bar t)
  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (define-keymap :keymap org-mode-map
    "C-c o M" #'+org/toggle-emphasis-markers
    "C-c o m" #'org-modern-mode
    "M-g o"   #'consult-org-heading)
  (setopt org-export-backends '(html latex texinfo))
  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c"))))

(after-load! ox-latex
  (alist-setq! org-latex-classes
    "ctexart" '("\\documentclass[11pt]{ctexart}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after-load! org-capture
  (add-to-list 'org-capture-templates
               `("i" "Inbox" entry (file "inbox.org")
                 ,(concat "* TODO %?\n"
                          "/Entered on/ %U")))

  (add-to-list 'org-capture-templates
               `("m" "Meeting" entry (file+headline "agenda.org" "Future")
                 ,(concat "* %? :meeting:\n"
                          "<%<%Y-%m-%d %a %H:00>>")))

  (add-to-list 'org-capture-templates
               `("n" "Note" entry (file "notes.org")
                 ,(concat "* Note (%a)\n"
                          "/Entered on/ %U\n" "\n" "%?"))))

;;;; TeX

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook #'turn-on-reftex) ; AUCTeX
(add-hook 'latex-mode-hook #'turn-on-reftex) ; Emacs latex mode

;;;; telega

(after-load! telega
  (require 'telega-config))

;;;; ement

(setopt ement-save-sessions t)

;;;; rcirc

(after-load! rcirc
  (rcirc-track-minor-mode))

;;;; erc

(defvar erc-modules)
(defvar erc-mode-map)
(after-load! erc
  ;; This enables displaying servers and channels in side windows,
  ;; which can be toggled by C-x w s.
  (setopt erc-modules
          (seq-union '(sasl nicks scrolltobottom track)
                     erc-modules))

  ;; insert a newline when I hit <RET> at the prompt, and prefer
  ;; something more deliberate for actually send messages.
  (keymap-unset erc-mode-map "RET")
  (keymap-set erc-mode-map "C-c C-c" #'erc-send-current-line)

  ;; protect me from accidentally sending excess lines.
  (setq erc-inhibit-multiline-input t
        erc-send-whitespace-lines t
        erc-ask-about-multiline-input t)
  ;; scroll all windows to prompt when submitting input.
  (setq erc-scrolltobottom-all t)

  ;; reconnect automatically using a fancy strategy.
  (setq erc-server-reconnect-function
        #'erc-server-delayed-check-reconnect
        erc-server-reconnect-timeout 30)

  ;; show new buffers in the current window instead of a split.
  (setq erc-interactive-display 'buffer)

  ;; prefer one message line without continuation indicators.
  (setq erc-fill-function #'erc-fill-wrap
        erc-fill-static-center 18)
  (after-load! erc-fill
    (require 'erc-fill)
    (keymap-set erc-fill-wrap-mode-map "C-c =" #'erc-fill-wrap-nudge))

  ;; prevent JOINs and PARTs from lighting up the mode-line.
  (after-load! erc-track
    (setopt erc-track-faces-priority-list (remq 'erc-notice-face
                                                erc-track-faces-priority-list)))
  (setq erc-track-priority-faces-only 'all))

;;;; copilot

(after-load! copilot
  ;; TODO choose better keybindings
  (define-keymap :keymap copilot-mode-map
    "<tab>"   #'copilot-accept-completion
    "C-<tab>" #'copilot-accept-completion-by-word))

;;;; consult-browser-hist

(autoload 'consult-browser-hist "consult-browser-hist" nil t)
(keymap-global-set "M-s b" #'consult-browser-hist)

;;;; webjump

(after-load! webjump
  (alist-setq! webjump-sites
    "GitHub" "https://github.com"
    "CodeBerg" "https://codeberg.org"
    "Kagi Search" [simple-query "www.kagi.com"
                                "www.kagi.com/search?q=" ""]
    "Kagi Assistant" [simple-query "www.kagi.com/assistant/"
                                   "www.kagi.com/assistant?q=" ""]
    "yhetil.org" "https://yhetil.org"))

;;;; vundo

(after-load! vundo
  (setopt vundo-glyph-alist vundo-unicode-symbols))

;;;; markdown

(after-load! markdown-mode
  (keymap-set markdown-mode-map      "C-x C-q" #'markdown-view-mode)
  (keymap-set markdown-view-mode-map "C-x C-q" #'markdown-mode)
  (keymap-set gfm-mode-map           "C-x C-q" #'gfm-view-mode)
  (keymap-set gfm-view-mode-map      "C-x C-q" #'gfm-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode))

;;;; p-search

(after-load! p-search
  (require 'psx-info "extensions/p-search-x-info.el"))
(autoload 'p-search "p-search" nil t)

;;;; copyright

(setopt copyright-year-ranges t)
(add-hook 'before-save-hook #'copyright-update)

;;;; sftp
(autoload 'sftp "sftp" nil t)

;;;; mode-line

(blackout 'meow-insert-mode)
(blackout 'meow-normal-mode)
(blackout 'meow-motion-mode)
(blackout 'meow-beacon-mode)
(blackout 'meow-keypad-mode)
(after-load! ws-butler (blackout 'ws-butler-mode))
(after-load! paredit (blackout 'paredit-mode))
(after-load! whitespace (blackout 'whitespace-mode))
(after-load! goggles (blackout 'goggles-mode))
(after-load! jinx (blackout 'jinx-mode))
(after-load! gcmh (blackout 'gcmh-mode))
(after-load! eldoc (blackout 'eldoc-mode))
(after-load! valign (blackout 'valign-mode))
(after-load! abbrev (blackout 'abbrev-mode))
(after-load! olivetti (blackout 'olivetti-mode))
(after-load! nerd-icons-multimodal (blackout 'nerd-icons-multimodal-mode))
(after-load! clipetty (blackout 'clipetty-mode))

;;;; uptime

;; Set up a timer to display emacs uptime every 30 min.

(defun uptime-notify ()
  (message "Emacs has been running for %s" (emacs-uptime)))

(defvar uptime-notification-timer
  (run-with-timer 1800 1800 #'uptime-notify))

;;;; quilt

(defun quilt-new-patch (patch-name)
  (interactive "sNew patch name:")
  (unless (string-suffix-p "patch" patch-name)
    (setq patch-name (concat patch-name ".patch")))
  (shell-command (concat "quilt new " (shell-quote-argument patch-name))))

(defun quilt-add-visited-file ()
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (let ((basename (file-name-nondirectory file)))
        (shell-command (concat "quilt add " (shell-quote-argument basename))))
    (user-error "Buffer not visiting a file")))

(defun quilt-list-files ()
  (interactive)
  (shell-command "quilt files"))

(defun quilt-refresh ()
  (interactive)
  (shell-command "quilt refresh"))

(defun quilt-pop (arg)
  (interactive "p")
  (while (> arg 0)
    (shell-command "quilt pop")
    (cl-decf arg)))

(defun quilt-push (arg)
  (interactive "p")
  (while (> arg 0)
    (shell-command "quilt push")
    (cl-decf arg)))

(defun quilt-list-applied-patches ()
  (interactive)
  (shell-command "quilt applied"))

(defvar-keymap quilt-prefix-map
  :prefix 'quilt-prefix-map
  "n" #'quilt-new-patch
  "a" #'quilt-add-visited-file
  "-" #'quilt-pop
  "+" #'quilt-push
  "f" #'quilt-list-files
  "g" #'quilt-refresh
  "l" #'quilt-list-applied-patches)


;;;; display buffer alist


;;;; Custom commands
(defun find-early-init-file ()
  "Find `early-init-file'."
  (interactive)
  (find-file early-init-file))

(defun find-user-init-file ()
  "Find `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun +toggle-side-window (side &optional frame)
  "Toggle a side window on SIDE of FRAME.
When a side window exists on SIDE, close it and remember its state.
When no side window exists on SIDE, restore the remembered state.

SIDE is one of the symbols 'left, 'right, 'above, or 'below.
FRAME defaults to the selected frame."
  (let* ((frame (window-normalize-frame frame))
         (window--sides-inhibit-check t)
         (side-win (window-with-parameter 'window-side side frame))
         state)
    (cond
     (side-win
      (setf (alist-get side (frame-parameter frame 'side-window-states))
            (window-state-get side-win))
      (delete-window side-win))
     ((setq state (alist-get side (frame-parameter frame 'side-window-states)))
      (let* ((size (if (memq side '(left right))
                       (alist-get 'total-width state)
                     (alist-get 'total-height state)))
             (new-win (split-window (frame-root-window frame) (and size (- size)) side)))
        (window-state-put state new-win)))
     (t
      (user-error "No remembered side window for this frame")))))

(defun +toggle-side-window-left (&optional frame)
  "Toggle a side window on the left of FRAME.
When a left side window exists, close it and remember its state.
When no left side window exists, restore the remembered state.

FRAME defaults to the selected frame."
  (interactive)
  (+toggle-side-window 'left frame))

(defun +toggle-side-window-right (&optional frame)
  "Toggle a side window on the right of FRAME.
When a right side window exists, close it and remember its state.
When no right side window exists, restore the remembered state.

FRAME defaults to the selected frame."
  (interactive)
  (+toggle-side-window 'right frame))

(defun +toggle-side-window-above (&optional frame)
  "Toggle a side window above FRAME.
When an above side window exists, close it and remember its state.
When no above side window exists, restore the remembered state.

FRAME defaults to the selected frame."
  (interactive)
  (+toggle-side-window 'above frame))

(defun +toggle-side-window-below (&optional frame)
  "Toggle a side window below FRAME.
When a below side window exists, close it and remember its state.
When no below side window exists, restore the remembered state.

FRAME defaults to the selected frame."
  (interactive)
  (+toggle-side-window 'below frame))

(defvar +toggle-transparent-alpha 80)

(defun +toggle-transparent (&optional arg)
  "Toggle transparent background.
If ARG is 1 or unspecified, toggle transparent background.
If ARG is 4 or `unversal-argument', enable transparent background.
Otherwise disable it."
  (interactive "p")
  (let ((current-alpha (frame-parameter nil 'alpha-background)))
    (when (and (numberp current-alpha) (< current-alpha 100))
      (setq +toggle-transparent-alpha current-alpha))
    (cond ((eq arg 1)
           (if (and (numberp current-alpha) (< current-alpha 100))
               (modify-frame-parameters nil '((alpha-background . nil)))
             (modify-frame-parameters nil `((alpha-background . ,+toggle-transparent-alpha)))))
          ((eq arg 4)
           (modify-frame-parameters nil `((alpha-background . ,+toggle-transparent-alpha))))
          (t
           (modify-frame-parameters nil `((alpha-background . nil)))))))


;;;; keybindings

(defalias 'window-prefix-map window-prefix-map)
(defalias 'project-prefix-map project-prefix-map)

(define-keymap :keymap project-prefix-map
  "m" #'magit-project-status)

(defvar-keymap tool-map
  :doc    "Keymap for calling external tools."
  :prefix 'tool-map
  "A"     #'gptel
  "a"     #'gptel-menu
  "t"     #'tui-run
  "r"     #'tui-rg
  "R"     #'tui-recentf
  "g"     #'tui-ugrep
  "G"     #'tui-git-ls-files
  "y"     #'tui-yazi
  "k"     #'tui-kill
  "l"     #'tui-line
  "f"     #'tui-find
  "d"     #'tui-locate
  "p"     #'tui-switch-project
  "e"     #'eshell
  "s"     #'eat
  "c"     #'telega-chat-with)

(defvar-keymap doc-map
  :doc    "Documentation commands."
  :prefix 'doc-map
  "d"     #'devdocs-lookup
  "i"     #'devdocs-install
  "p"     #'devdocs-peruse
  "r"     #'rust-docs-lookup
  "g"     #'good-doc-lookup)

(defvar-keymap file-map
  :doc    "Open file commands."
  :prefix 'file-map
  "e"     #'find-early-init-file
  "f"     #'find-file
  "i"     #'find-user-init-file
  "a"     #'ffap
  "r"     #'ff-find-related-file
  "R"     #'consult-recent-file
  "n"     #'rename-visited-file
  "b"     #'backup-list-backups)

(defvar-keymap toggle-map
  :doc    "Keymap for toggling options."
  :prefix 'toggle-map
  "e"     #'eglot
  "f"     #'flymake-mode
  "q"     #'display-fill-column-indicator-mode
  "l"     #'display-line-numbers-mode
  "o"     #'outline-minor-mode
  "c"     #'+visual-fill-column/toggle-visual-fill-and-center
  "C"     #'olivetti-mode
  "x"     #'+toggle-transparent
  "v"     #'visual-line-mode
  "w"     #'whitespace-mode
  "t"     #'consult-theme)

(defvar-keymap debug-map
  :doc    "Keymap for debugging commands."
  :prefix 'debug-map
  "e"     #'toggle-debug-on-error
  "q"     #'toggle-debug-on-quit
  "f"     #'debug-on-entry
  "v"     #'debug-on-variable-change
  "c f"   #'cancel-debug-on-entry
  "c v"   #'cancel-debug-on-variable-change)

(define-keymap :keymap mode-specific-map
  "A"   #'org-agenda
  "C"   #'org-capture
  "D"   debug-map
  "E"   #'elfeed
  "L"   #'org-store-link
  "S"   straight-prefix-map
  "T"   #'telega
  "G"   #'gnus
  "V"   #'vundo
  "P"   #'p-search
  "a"   #'embark-act
  "t"   tool-map
  "d"   doc-map
  "e"   #'cape-prefix-map
  "f"   file-map
  "j"   #'webjump
  "o"   toggle-map
  "M-g" #'magit-file-dispatch
  "p"   #'project-prefix-map
  "q"   quilt-prefix-map
  "r"   #'rg-dwim
  "R"   #'rg-menu
  "s"   search-map
  "v"   #'vc-prefix-map
  "w"   #'window-prefix-map
  "C-'" #'claude-code-ide-menu)

(define-keymap :keymap ctl-x-4-map
  "t" #'eat-other-window
  "e" #'+eshell/other-window)

(define-keymap :keymap window-prefix-map
  "f 2" #'window-layout-flip-topdown
  "f 3" #'window-layout-flip-leftright
  "r r" #'window-layout-rotate-clockwise
  "r l" #'window-layout-rotate-anticlockwise
  "h"   #'+toggle-side-window-left
  "l"   #'+toggle-side-window-right
  "k"   #'+toggle-side-window-above
  "j"   #'+toggle-side-window-below)

(define-keymap :keymap ctl-x-map
  "F"   #'find-function
  "V"   #'find-variable
  "K"   #'find-function-on-key
  "L"   #'find-library
  "4 F" #'find-function-other-window
  "4 V" #'find-variable-other-window
  "4 K" #'find-function-on-key-other-window
  "4 L" #'find-library-other-window
  "5 F" #'find-function-other-frame
  "5 V" #'find-variable-other-frame
  "5 K" #'find-function-on-key-other-frame
  "5 L" #'find-library-other-frame
  "k"   #'kill-current-buffer
  "g"   #'magit-status-quick
  "M-g" #'magit-dispatch)

(define-keymap :keymap global-map
  "C-h"    "DEL"
  "C-M-h"  "M-DEL"
  "M-c"    #'capitalize-dwim
  "M-l"    #'downcase-dwim
  "M-u"    #'upcase-dwim
  "<f5>"   #'project-recompile)


;;;; post-init

(defvar post-init-file (locate-user-emacs-file "post-init.el"))

(when (file-exists-p post-init-file)
  (let ((straight-current-profile 'user))
    (load post-init-file nil t)))

;;;; _

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
