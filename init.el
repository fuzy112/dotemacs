;;; init.el --- Emacs configuration file           -*- lexical-binding: t; -*-

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

(require 'early-init early-init-file t)
(when (featurep 'init)
  (load early-init-file nil t))
(eval-when-compile
  (require 'dotemacs-core)
  (require 'compat))

;;;; pre-init

(defvar pre-init-file (locate-user-emacs-file "pre-init.el")
  "The file to load before the init file.")

(defvar straight-current-profile)
(when (file-exists-p pre-init-file)
  (let ((straight-current-profile 'user))
    (load pre-init-file nil t)))


(defvar custom-file)
(when (file-exists-p custom-file)
  (load custom-file))
;;;; meow-edit

(require 'meow)

(defun meow-setup ()
  "Setup meow keybindings for Dvorak layout."
  (setq meow-keypad-leader-dispatch "C-c")
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
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
   '("?" . meow-cheatsheet))
  (meow-motion-overwrite-define-key '("<escape>" . ignore))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("(" . meow-backward-slurp)
   '(")" . meow-forward-slurp)
   '("{" . meow-backward-barf)
   '("}" . meow-forward-barf)))

(meow-setup)
(meow-global-mode)

(when (treesit-available-p)
  (meow-tree-sitter-register-defaults))


;;;; straight

(defvar straight--recipe-cache)
(defun straight-magit-package-status (pkg)
  "Run `magit-dispatch' in the repo of PKG."
  (interactive (list (straight--select-package "Visit: ")))
  (let ((repo (plist-get (gethash pkg straight--recipe-cache)
                         :local-repo)))
    (magit-status-setup-buffer (straight--repos-dir repo))))

(autoload 'straight-x-fetch-all "straight-x" nil t)

(defvar-keymap straight-prefix-map
  :doc "Prefix map for straight.el commands."
  :prefix 'straight-prefix-map
  :name "Straight map"
  "c" #'straight-check-package
  "C" #'straight-check-all
  "p" #'straight-pull-package
  "P" #'straight-pull-all
  "f" #'straight-fetch-package
  "F" #'straight-fetch-all
  "b" #'straight-rebuild-package
  "B" #'straight-rebuild-all
  "v" #'straight-freeze-versions
  "V" #'straight-thaw-versions
  "n" #'straight-normalize-package
  "N" #'straight-normalize-all
  "m" #'straight-merge-package
  "M" #'straight-merge-all
  "u" #'straight-use-package
  "d" #'straight-visit-package
  "w" #'straight-visit-package-website
  "g" #'straight-magit-package-status
  "x f" #'straight-x-fetch-all)


;;;; fonts

;; The following fonts need to be installed:n
;;  - https://github.com/be5invis/Iosevka/releases/download/v31.8.0/SuperTTC-SGr-IosevkaSS04-31.8.0.zip
;;  - https://github.com/be5invis/Sarasa-Gothic/releases/download/v1.0.26/Sarasa-SuperTTC-1.0.26.7z
;;  - https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/NerdFontsSymbolsOnly.zip

(unless (eq (framep-on-display) t)
  (with-demoted-errors "Failed to setup fonts: %S"

    ;; chinese
    (set-fontset-font (frame-parameter nil 'font) 'han "Sarasa Gothic CL")
    (set-fontset-font (frame-parameter nil 'font) 'cjk-misc "Sarasa Gothic CL")

    ;; nerd-icons
    (let ((charsets '((#xe5fa . #xe6b2)  ;; Seti-UI + Custom
		      (#xe700 . #xe7c5)  ;; Devicons
		      (#xf000 . #xf2e0)  ;; Font Awesome
		      (#xe200 . #xe2a9)  ;; Font Awesome Extension
		      (#xf500 . #xfd46) (#xf0001 . #xf1af0) ;; Material Design Icons
		      (#xe300 . #xe3eb)  ;; Weather
		      (#xf400 . #xf4a8) #x2665 #x26a1 #xf27c  ;; Octicons
		      (#xe0a0 . #xe0a2) (#xe0b0 . #xe0b3)  ;; Powerline Symbols
		      #xe0a3 (#xe0b4 . #xe0c8) (#xe0cc . #xe0d2) #xe0d4  ;; Powerline Extra Symbols
		      (#x23fb . #x23fe) #x2b58  ;; IEC Power Symbols
		      (#xf300 . #xf372)  ;; Font Logos
		      (#xe000 . #xe00a)  ;; Pomicons
		      (#xea60 . #xebeb))))  ;; Codicons
      (cl-loop for charset in charsets do
	       (set-fontset-font
	        (frame-parameter nil 'font)
	        charset
                "Symbols Nerd Font"
	        nil
	        'prepend)))

    (set-fontset-font t 'han "Sarasa Gothic CL")))

(set-face-attribute 'default nil :family "Iosevka SS04")
(set-face-attribute 'fixed-pitch nil :family "Iosevka SS04")
(set-face-attribute 'variable-pitch nil :family "Sarasa UI CL")

(setopt face-font-family-alternatives
        (seq-union '(("Sarasa Gothic CL" "Iosevka SS04")
                     ("Sarasa UI CL" "Sarasa Gothic CL" "Iosevka SS04"))
                   face-font-family-alternatives))


;;;; modus-theme

(after-load! modus-themes
  (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (setq modus-themes-mixed-fonts t
	modus-themes-bold-constructs t
	modus-themes-slanted-constructs t
	modus-themes-variable-pitch-ui t))

(when (not custom-enabled-themes)
  (load-theme 'modus-operandi :no-confirm))

;;;; doom-modeline

(require 'doom-modeline)
(doom-modeline-mode)

;;;; customized faces
(defun +custom-faces (&optional theme)
  (unless (eq theme 'user)
    (defvar pp-posframe-parameters)
    (setq pp-posframe-parameters `( :border-color "gray"
				    :border-width 1
				    :background-color ,(face-background 'default nil '(shadow))))
    (custom-set-faces
     `(fill-column-indicator
       ((((type w32 tty))
	 :height 1.0 :foreground "gray50" :background ,(face-background 'default))))
     '(parenthesis
       ((t :inherit shadow)))
     `(header-line
       ((((supports :underline t) (class color grayscale))
	 :background ,(face-background 'default)
         :underline (:color ,(face-foreground 'default) :style line :position t)
	 :box (:line-width 6 :color ,(face-background 'default) :style nil))))
     `(mode-line-active
       ((((supports :overline t) (class color grayscale))
	 :background ,(face-background 'default)
         :foreground ,(face-foreground 'default)
         :overline t
	 :box (:line-width 6 :color ,(face-background 'default) :style nil))))
     `(mode-line-inactive
       ((((supports :overline t) (class color grayscale))
	 :background ,(face-background 'default)
         :foreground ,(face-foreground 'shadow)
         :overline t
	 :box (:line-width 6 :color ,(face-background 'default) :style nil)))))))

(+custom-faces)
(add-hook 'enable-theme-functions #'+custom-faces t)

;;;; libraries
(after-load! (:or dash elisp-mode)
  (global-dash-fontify-mode))
(after-load! info-look
  (dash-register-info-lookup))
(after-load! (:and anaphora elisp-mode)
  (anaphora-install-font-lock-keywords))


;;;; nerd-icons

(add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-mode)
(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
(add-hook 'dired-mode-hook #'nerd-icons-multimodal-mode)
(add-hook 'archive-mode-hook #'nerd-icons-multimodal-mode)
(add-hook 'tar-mode-hook #'nerd-icons-multimodal-mode)

;;;; pixel-scroll

(defun +pixel-scroll--autoload ()
  (interactive)
  (require 'pixel-scroll)
  (let ((events (mapcar (lambda (ev) (cons t ev))
                        (listify-key-sequence (this-command-keys)))))
    (setq unread-command-events (append events unread-command-events))))
(keymap-global-set "<wheel-down>" #'+pixel-scroll--autoload)
(keymap-global-set "<wheel-up>" #'+pixel-scroll--autoload)
(after-load! pixel-scroll
  (keymap-global-unset "<wheel-down>")
  (keymap-global-unset "<wheel-up>")
  (fmakunbound '+pixel-scroll--autoload)
  (pixel-scroll-precision-mode))

;;;; window

(setopt kill-buffer-quit-windows t
        quit-restore-window-no-switch t)

;;;; quick-window

(autoload 'quick-window-jump "quick-window" nil t)
(keymap-global-set "M-o" #'quick-window-jump)

;;;; help

(after-load! help
  (setq help-enable-variable-value-editing t
        help-enable-completion-autoload nil)
  (when (fboundp 'shortdoc-help-fns-examples-function)
    (add-hook 'help-fns-describe-function-functions
              #'shortdoc-help-fns-examples-function 50))

  (add-to-list 'help-fns-describe-function-functions #'help-fns-function-source-code 'append))

(defun help-fns-function-source-code (function)
  "Insert Emacs Lisp source code for FUNCTION into the current buffer."
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
    (add-text-properties 0 (length text)
                         '(line-prefix (space :align-to 2))
                         text)
    (insert "\n  Source code:\n\n")
    (insert text)
    (insert "\n\n")))

;;;; emacs-lock-mode

;; prevent emacs from exiting if the *scratch* buffer is changed.
(with-current-buffer "*scratch*"
  (add-hook 'first-change-hook #'emacs-lock-mode nil t))

;;;; backup

(autoload 'list-backups "backup" nil t)
(autoload 'backup-list-backups "backup" nil t)

;;;; breadcrumb

(add-hook 'text-mode-hook #'breadcrumb-local-mode)
(add-hook 'conf-mode-hook #'breadcrumb-local-mode)
(add-hook 'prog-mode-hook #'breadcrumb-local-mode)

;;;; orderless

(eval-when-compile (require 'orderless))
(orderless-define-completion-style orderless+flex
  (orderless-matching-styles '(orderless-flex)))
(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file . ((styles . (basic partial-completion))))
        (symbol . ((styles . (orderless+flex))))
        (symbol-help . ((styles . (orderless+flex))))
        (command . ((styles . (orderless+initialism))))
        (variable . ((styles . (orderless+initialism))))
        (eglot . ((styles . (orderless))))
        (eglot-capf . ((styles . (orderless))))
        (magit-rev . ((styles . (orderless+flex))))))

;; copied from orderless wiki:
(defun +orderless--consult-suffix ()
  "Regexp which matches the end of string with Consult tofu support."
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$" consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

(defun +orderless--consult-dispatch (word _index _total)
  (cond
   ;; Ensure that $ works with consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

(after-load! 'orderless
  (add-to-list 'orderless-style-dispatchers #'+orderless--consult-dispatch))

;;;; vertico

(setq enable-recursive-minibuffers t)

(autoload 'vertico--advice "vertico.el")
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
    "M-P" #'vertico-repeat-previous
    "M-N" #'vertico-repeat-next
    ;; vertico-directory
    "RET" #'vertico-directory-enter
    "DEL" #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    ;; vertico-quick
    "C-q" #'vertico-quick-insert
    "M-q" #'vertico-quick-exit))

;;;; marginalia

(autoload 'marginalia--minibuffer-setup "marginalia.el")
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
        (cdr args)))

;;;; cursor-sensor

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq minibuffer-prompt-properties '(read-only t face minibuffer-prompt cursor-intangible t))

;;;; pulsar

(when (display-graphic-p)
  (pulsar-global-mode))

;;;; goggles

(add-hook 'prog-mode-hook #'goggles-mode)
(add-hook 'text-mode-hook #'goggles-mode)

;;;; corfu

(define-advice completion-in-region (:before (&rest _) corfu)
  (require 'corfu))

(defvar corfu-map)
(after-load! corfu
  (advice-remove 'completion-in-region #'completion-in-region@corfu)
  (require 'orderless)
  (setopt completion-cycle-threshold 0
          tab-always-indent 'complete
          text-mode-ispell-word-completion nil
          read-extended-command-predicate #'command-completion-default-include-p)
  (setopt corfu-cycle t
          corfu-preselect 'prompt)
  (setopt corfu-quick1 "htnsd"
          corfu-quick2 "ueoai")
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (define-keymap :keymap corfu-map
    "C-q" #'corfu-quick-insert
    "M-q" #'corfu-quick-complete
    "TAB" #'corfu-next
    "S-TAB" #'corfu-previous
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
  "Apply ansi color sequence on region [BEG END).

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
  (setq embark-cycle-key ".")
  (setq embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  (keymap-set embark-file-map "#" '+embark/find-file-as-root)
  (keymap-set embark-bookmark-map "W" '+embark/eww-open-bookmark)
  (keymap-set embark-bookmark-map "u" '+embark/browse-url-open-bookmark)
  (keymap-set embark-region-map "[" '+embark/apply-ansi-color))

;;;; consult

(define-keymap
  :keymap global-map
  "C-c M-x" #'consult-mode-command
  "C-c h" #'consult-history
  "C-c k" #'consult-kmacro
  "C-c m" #'consult-man
  "C-c i" #'consult-info
  "<remap> <Info-search>" #'consult-info
  ;; C-x bindings in `ctl-x-map'
  "C-x M-:" #'consult-complex-command ;; orig. repeat-complex-command
  "C-x b" #'consult-buffer ;; orig. switch-to-buffer
  "C-c b" #'consult-buffer
  "C-x 4 b" #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
  "C-x 5 b" #'consult-buffer-other-frame ;; orig. switch-to-buffer-other-frame
  "C-x t b" #'consult-buffer-other-tab ;; orig. switch-to-buffer-other-tab
  "C-x r b" #'consult-bookmark         ;; orig. bookmark-jump
  "C-x p b" #'consult-project-buffer ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  "M-#" #'consult-register-load
  "M-'" #'consult-register-store ;; orig. abbrev-prefix-mark (unrelated)
  "C-M-#" #'consult-register
  ;; Other custom bindings
  "M-y" #'consult-yank-pop ;; orig. yank-pop
  ;; M-g bindings in `goto-map'
  "M-g e" #'consult-compile-error
  "M-g f" #'consult-flymake ;; Alternative: consult-flycheck
  "M-g g" #'consult-goto-line   ;; orig. goto-line
  "M-g M-g" #'consult-goto-line ;; orig. goto-line
  "M-g o" #'consult-outline ;; Alternative: consult-org-heading
  "M-g m" #'consult-mark
  "M-g k" #'consult-global-mark
  "M-g i" #'consult-imenu
  "M-g I" #'consult-imenu-multi
  ;; M-s bindings in `search-map'
  "M-s d" #'consult-find ;; Alternative: consult-fd
  "M-s c" #'consult-locate
  "M-s g" #'consult-grep
  "M-s G" #'consult-git-grep
  "M-s r" #'consult-ripgrep
  "M-s l" #'consult-line
  "M-s L" #'consult-line-multi
  "M-s k" #'consult-keep-lines
  "M-s u" #'consult-focus-lines
  ;; Isearch integration
  "M-s e" #'consult-isearch-history)

(define-keymap :keymap isearch-mode-map
  "M-e" #'consult-isearch-history
  "M-s e" #'consult-isearch-history
  "M-s l" #'consult-line
  "M-s L" #'consult-line-multi)

(define-keymap :keymap minibuffer-local-map
  "M-s" #'consult-history
  "M-r" #'consult-history)

(after-load! consult
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode))

(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)

(declare-function consult--read-1 "consult.el" (arg1 &rest rest))
(declare-function consult--file-preview "consult.el")

(defun +consult--read-file-name-function (prompt &optional dir _default mustmatch initial pred)
  (let* ((default-directory (abbreviate-file-name (or dir default-directory)))
         (minibuffer-completing-file-name t)
         (pred (or pred 'file-exists-p)))
    (require 'consult)
    ;; Use consult--read-1 instead of consult--read to suppress consult customizations.
    ;; TODO figure out if this is the correct way
    ;; should I bind this-command temporarily?
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

(declare-function consult--buffer-preview "consult.el")
(defvar consult-preview-key)

(defun +consult--read-buffer-function (prompt &optional def mustmatch pred)
  (require 'consult)
  ;; Use consult--read-1 instead of consult--read to suppress consult customizations.
  ;; TODO figure out if this is the correct way
  ;; should I bind this-command temporarily?
  (consult--read-1
   #'internal-complete-buffer
   :state (consult--buffer-preview)
   :default def
   :prompt (format-prompt (replace-regexp-in-string ":[[:space:]]*\\'" "" prompt) def)
   :require-match mustmatch
   :predicate pred
   :preview-key consult-preview-key
   :sort t
   :lookup (lambda (selected &rest _) selected)))

(setq read-buffer-function #'+consult--read-buffer-function)

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
  (eval '(consult-customize
          consult-xref consult-ripgrep consult-grep consult-git-grep
          consult-line consult-focus-lines consult-keep-lines
          consult-imenu
          :preview-key '(:debounce 0.2 any)))

  ;; url-only bookmark type
  (cl-pushnew #'url-bookmark-jump (cddr (assoc ?w consult-bookmark-narrow))))


(defun +embark-consult-export-grep--headings (&rest _)
  "Group the results of `embark-consult-export-grep'."
  ;; `grep--heading-filter' is a new function in Emacs 30.1.
  (when (fboundp 'grep--heading-filter)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (grep--heading-filter)))))

(declare-function embark-consult-export-grep "embark-consult.el" (arg1))
(after-load! embark-consult
  (require 'grep)
  (when (fboundp 'grep--heading-filter)
    (advice-add #'embark-consult-export-grep :after #'+embark-consult-export-grep--headings)))


(keymap-global-set "C-x C-d" #'consult-dir)
(define-keymap :keymap minibuffer-local-map
  "C-x C-d" #'consult-dir
  "C-x C-j" #'consult-dir-jump-file)

;;;; indent-aux

;; New minor mode in Emacs 30.1: deindents code copied into kill-ring.
(when (fboundp 'kill-ring-deindent-mode)
  (kill-ring-deindent-mode))

;;;; windmove

(define-keymap :keymap global-map
  "S-<left>" #'windmove-left
  "S-<right>" #'windmove-right
  "S-<up>" #'windmove-up
  "S-<down>" #'windmove-down)

;;;; popper

(autoload 'popper-cycle "popper.el" nil t)
(autoload 'popper-toggle-type "popper.el" nil t)
(autoload 'popper-toggle "popper.el" nil t)
(define-keymap :keymap global-map
  "C-`" #'popper-toggle
  "M-`" #'popper-cycle
  "C-M-`" #'popper-toggle-type)
(defvar popper-reference-buffers)
(setq popper-reference-buffers '("\\*Messages\\*" "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   "-eat\\*$"
                                   "-vterm\\*$"
                                   ;; "\\*Warnings\\*"
                                   ;; "\\*Compile-Log\\*"
                                   "\\*vc-git : "
                                   ;;xref--xref-buffer-mode
                                   help-mode compilation-mode
                                   flymake-diagnostics-buffer-mode))

(declare-function popper-display-control-p "popper.el" (buf &optional _act))
(defun +popper--popup-p (buffer-or-name &optional arg)
  (and (seq-some
        (lambda (it)
          (buffer-match-p
           (cond
            ((stringp it) it)
            ((symbolp it) `(derived-mode . ,it))
            ((functionp it) it)
            (t (error "Invalid element in `popper-reference-buffers': %S" it)))
           buffer-or-name
           arg))
        popper-reference-buffers)
       (progn
         (require 'popper)
         (popper-display-control-p buffer-or-name arg))))

(defvar popper-display-function)
(defun +popper--display (buffer alist)
  (funcall popper-display-function buffer alist))

(alist-setq! display-buffer-alist +popper--popup-p '(+popper--display))

(after-load! popper
  (alist-delq! display-buffer-alist +popper--popup-p)
  (after-load! project
    (setq popper-group-function #'popper-group-by-project))
  (popper-mode)
  (popper-echo-mode))

;;;; ibuffer

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-jump)

;;;; apheleia

(keymap-global-set "C-x x /" #'apheleia-format-buffer)
(keymap-global-set "C-c C-/" #'apheleia-format-buffer)

;;;; ws-butler

(add-hook 'find-file-hook #'ws-butler-mode)
(after-load! ws-butler
  (remove-hook 'find-file-hook #'ws-butler-mode)
  (ws-butler-global-mode))

;;;; whitespace

(dolist (hook '(prog-mode-hook conf-mode-hook yaml-mode-hook))
  (add-hook hook #'whitespace-mode))
(setopt whitespace-style '(face trailing empty indentation space-before-tab space-after-tab))

;;;; recentf

(autoload 'recentf-track-opened-file "recentf.el"
  "Insert the name of the file just opened or written into the recent list." )
(add-hook 'find-file-hook #'recentf-track-opened-file)
(after-load! recentf
  (setq recentf-max-saved-items 128)
  (let ((inhibit-message t))
    (recentf-mode)))
(after-load! consult
  ;; enable recentf when accessing the recentf-file source from consult.
  (setf (plist-get consult--source-recent-file :enabled)
        (lambda () (require 'recentf) (symbol-value 'recentf-mode))))

;;;; saveplace

(autoload 'save-place-find-file-hook "saveplace.el")
(autoload 'save-place-dired-hook "saveplace.el")
(add-hook 'find-file-hook #'save-place-find-file-hook)
(add-hook 'dired-initial-position-hook #'save-place-dired-hook)
(after-load! saveplace
  (save-place-mode))

;;;; autorevert

(autoload 'auto-revert--global-adopt-current-buffer "autorevert.el")
(add-hook 'find-file-hook #'auto-revert--global-adopt-current-buffer)

(after-load! autorevert
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode))

;;;; dired

(add-hook 'dired-mode-hook #'dired-omit-mode)
(after-load! dired
  (setq dired-listing-switches "-lah"
        dired-hide-details-hide-absolute-location t
        dired-x-hands-off-my-keys nil))

;;;; compile

(defun +process-use-pipe ()
  (setq-local process-connection-type nil))

(after-load! compile
  (add-hook 'compilation-mode-hook #'+process-use-pipe)
  (setq compilation-always-kill t
        compilation-ask-about-save t
        compilation-scroll-output 'first-error))

;;;; comint

(after-load! comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048))

;;;; eshell

(defun +eshell/here ()
  "Open an EShell buffer in `default-directory'."
  (interactive)
  (defvar eshell-buffer-name)
  (with-suppressed-warnings ((obsolete display-comint-buffer-action))
    (defvar display-comint-buffer-action)
    (let ((eshell-buffer-name (format "*%s : eshell*" (abbreviate-file-name default-directory)))
          (display-comint-buffer-action '(() (inhibit-same-window . t))))
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
      (keymap-set eshell-hist-mode-map "M-r" #'consult-history))))

;;;; text-mode

(defun +text-mode--capf ()
  "Addd `cape-dict' to the buffer local value of `completion-at-point-functions'."
  (when (fboundp 'cape-dict)
    (add-hook 'completion-at-point-functions #'cape-dict 90 t)))
(add-hook 'text-mode-hook #'+text-mode--capf)

;;;; nxml-mode

(defun +nxml-mode--flymake ()
  (when (fboundp 'flymake-xmllint)
    (add-hook 'flymake-diagnostics-functions nil #'flymake-xmllint)))
(add-hook 'nxml-mode-hook #'+nxml-mode--flymake)

;;;; outline

(defun +outline-minor-faces ()
  "Enable `outline-minor-faces-mode' if not in a `help-mode' buffer."
  ;; outline-minor-faces-mode conflicts with \\[describe-mode].
  (unless (derived-mode-p 'help-mode)
    (outline-minor-faces-mode)))
(add-hook 'outline-minor-mode-hook '+outline-minor-faces)

;;;; adaptive-wrap or visual-wrap

;; visual-wrap is a built-in replacement of adaptive-wrap since Emacs
;; 30.1.
(static-if (locate-library "visual-wrap")
    (add-hook 'visual-line-mode-hook #'visual-wrap-prefix-mode)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

;;;; hl-todo

(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'conf-mode-hook #'hl-todo-mode)

(after-load! hl-todo
  (define-keymap :keymap hl-todo-mode-map
    "C-c t [" #'hl-todo-previous
    "C-c t ]" #'hl-todo-next
    "C-c t o" #'hl-todo-occur
    "C-c t i" #'hl-todo-insert)
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

(after-load! eglot
  (eglot-tempel-mode))

;;;; dynamic-highlight

(autoload 'dynamic-highlight-mode "dynamic-highlight.el" nil t)
(add-hook 'c-mode-common-hook #'dynamic-highlight-mode)
(after-load! dynamic-highlight
  (setq dynamic-highlight-predicate (lambda (_) (and (not (bound-and-true-p eglot-mode))
                                                (not (puni--in-comment-p))))))

;;;; xref

;; use Ctrl and mouse click to jump to definitions, and Ctrl+Alt+mouse
;; click to jump to references.
(keymap-global-unset "C-<down-mouse-1>")
(keymap-global-unset "C-M-<down-mouse-1>")
(keymap-global-set "C-<mouse-1>" #'xref-find-definitions-at-mouse)
(keymap-global-set "C-M-<mouse-1>" #'xref-find-references-at-mouse)

(defvar +xref--max-definitions-in-buffer 5)

(declare-function xref-show-definitions-buffer-at-bottom "xref.el")
(defun +xref--show-definition (fetcher alist)
  "Use `xref-show-definitions-buffer' if the candidates are few.
Otherwise use `consult-xref'.

See `xref-show-xrefs' for FETCHER and ALIST."
  (let ((xrefs (funcall fetcher)))
    (if (length< xrefs +xref--max-definitions-in-buffer)
        (xref-show-definitions-buffer-at-bottom fetcher alist)
      (consult-xref fetcher alist))))

(after-load! xref
  (setq xref-search-program
        (cond ((executable-find "rg") 'ripgrep)
              ((executable-find "ugrep") 'ugrep)
              (t 'grep))
        xref-show-definitions-function #'+xref--show-definition
        xref-auto-jump-to-first-definition t))

;;;; ctags

(autoload 'ctags-menu "ctags-menu" nil t)
(autoload 'ctags-xref-backend "ctags-xref")
(add-hook 'xref-backend-functions #'ctags-xref-backend)

;; Override `xref-backend-references' for ctags: if GRTAGS exists,
;; find reference using `gtags' backend.
(cl-defmethod xref-backend-references :around ((_backend (eql 'ctags)) identifier)
  (require 'gtags)
  (if (and (locate-dominating-file default-directory "GRTAGS")
           (executable-find "global" t))
      (xref-backend-references 'gtags identifier)
    (cl-call-next-method)))

(after-load! ctags-xref
  (after-load! cc-mode
    (require 'ctags-xref-c)))

;;;; gtags

(autoload 'gtags-update "gtags.el" nil t)
(autoload 'gtags-single-update "gtags.el" nil)
(add-hook 'after-save-hook #'gtags-single-update)

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

;;;; paren-face

(add-hook 'lisp-data-mode-hook #'paren-face-mode)
(add-hook 'scheme-mode-hook #'paren-face-mode)

;;;; puni

(add-hook 'prog-mode-hook #'puni-mode)
(add-hook 'conf-mode-hook #'puni-mode)
(add-hook 'puni-mode-hook #'electric-pair-local-mode)
(after-load! elisp-mode
  (define-keymap :keymap emacs-lisp-mode-map
    "C-)" #'puni-slurp-forward
    "C-(" #'puni-slurp-backward
    "C-}" #'puni-barf-forward
    "C-{" #'puni-barf-backward))

(after-load! elec-pair
  (keymap-set electric-pair-mode-map "]" #'up-list))

;;;; elisp-mode

(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook #'flymake-straight-flymake-elisp-mode-init)
(after-load! elisp-mode
  ;; Emacs 30.1
  (when (boundp 'trusted-content)
    ;; trust contents in site-lisp
    (add-to-list 'trusted-content (locate-user-emacs-file "site-lisp/")))
  (when (native-comp-available-p)
    (keymap-set emacs-lisp-mode-map "C-c C-l" #'emacs-lisp-native-compile-and-load))
  (keymap-set lisp-interaction-mode-map "C-c C-j" #'eval-print-last-sexp))

;;;; pp

(keymap-global-set "M-:" #'pp-eval-expression)

;;;; pp-posframe

(autoload 'pp-posframe-eval-last-sexp "pp-posframe.el"
  "Evaluate sexp before point; display the value in a posframe." t)
(autoload 'pp-posframe-compile-defun "pp-posframe.el"
  "Compile and evaluate the current top-level form.
Display the result in a posframe." t)
(autoload 'pp-posframe-macroexpand-last-sexp "pp-posframe.el"
  "Macroexpand the sexp before point; display the result in a posframe." t)
(keymap-global-set "C-x C-e" #'pp-posframe-eval-last-sexp)
(after-load! elisp-mode
  (keymap-set emacs-lisp-mode-map "C-M-x" #'pp-posframe-compile-defun)
  (keymap-set emacs-lisp-mode-map "C-c M-e" #'pp-posframe-macroexpand-last-sexp))

;;;; find-func

(define-keymap :keymap ctl-x-map
  "F" #'find-function
  "V" #'find-variable
  "K" #'find-function-on-key
  "L" #'find-library
  "4 F" #'find-function-other-window
  "4 V" #'find-variable-other-window
  "4 K" #'find-function-on-key-other-window
  "4 L" #'find-library-other-window
  "5 F" #'find-function-other-frame
  "5 V" #'find-variable-other-frame
  "5 K" #'find-function-on-key-other-frame
  "5 L" #'find-library-other-frame)

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
(autoload 'flymake-clang-tidy "flymake-clang-tidy.el")
(defun +cc-mode--hook ()
  (add-hook 'flymake-diagnostics-functions #'flymake-clang-tidy nil t))
(add-hook 'c-mode-common-hook '+cc-mode--hook)

;;;; rust-mode

(declare-function project-prefixed-buffer-name "project.el" (arg1))
(define-advice rust--compile (:around (&rest args) project-prefix-buffer-name)
  (let ((compilation-buffer-name-function #'project-prefixed-buffer-name))
    (apply args)))

;;;; ruby

;;;; sh-script

(after-load! sh-script
  (add-hook 'sh-mode-hook #'sh-electric-here-document-mode))
(add-hook 'save-file-hook #'executable-make-buffer-file-executable-if-script-p)

;;;; project

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

  (dolist (file '(".project-root" "configure.ac" "Cargo.toml" "package.json"))
    (add-to-list 'project-vc-extra-root-markers file))
  (when (and (not (functionp project-switch-commands))
             (consp project-switch-commands))
    (add-to-list 'project-switch-commands '(project-compile "Compile") t)))

;;;; buffer-env

(add-hook 'hook-local-variables-hook #'buffer-env-update)
(add-hook 'comint-mode-hook #'buffer-env-update)
(defvar buffer-env--cache)
(defun +buffer-env/clear-cache ()
  "Clear buffer-env cache."
  (interactive)
  (clrhash buffer-env--cache))
(defvar buffer-env-command-alist)
(after-load! buffer-env
  (alist-setq! buffer-env-command-alist "/\\.nvmrc\\'" "~/.nvm/nvm-exec env -0" #'equal)
  (setopt buffer-env-script-name '(".envrc" ".nvmrc" ".env")))

;;;; vc

(after-load! vc
  (require 'diff-hl)
  (setq vc-follow-symlinks t)
  (setq vc-svn-global-switches
        '("--force-interactive"
          "--config-option"
          "config:auth:password-stores=gpg-agent")
        vc-svn-diff-switches '("-x" "-u -p")))

;;;; magit

(after-load! project
  (when (consp project-switch-commands)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))
(defvar magit-credential-cache-daemon-process)
(defun +magit--ccdp-no-query ()
  "Avoid query process status on exit."
  (when magit-credential-cache-daemon-process
    (set-process-query-on-exit-flag
     magit-credential-cache-daemon-process nil)))
(after-load! magit
  (advice-add #'magit-maybe-start-credential-cache-daemon :after '+magit--ccdp-no-query)
  (setopt magit-wip-mode-lighter "")
  (magit-wip-mode))

;;;; diff-hl

(autoload 'diff-hl-magit-post-refresh "diff-hl.el")
(add-hook 'tty-setup-hook #'diff-hl-margin-mode)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(after-load! diff-hl
  (setopt diff-hl-update-async t)
  (keymap-set diff-hl-mode-map "C-c v" diff-hl-command-map)
  (global-diff-hl-mode))

;;;; eldoc-diffstat

(add-hook 'magit-mode-hook #'eldoc-diffstat-mode)
(add-hook 'magit-blame-mode-hook #'eldoc-diffstat-mode)
(add-hook 'vc-annotate-mode-hook #'eldoc-diffstat-mode)

;;;; eat

(defvar eat-semi-char-non-bound-keys)
(setq eat-semi-char-non-bound-keys
      '(;; default non-bound keys
        [?\C-x] [?\C-\\] [?\C-q] [?\C-g] [?\C-h] [?\e ?\C-c] [?\C-u]
        [?\e ?x] [?\e ?:] [?\e ?!] [?\e ?&]
        [C-insert] [M-insert] [S-insert] [C-M-insert]
        [C-S-insert] [M-S-insert] [C-M-S-insert]
        [C-delete] [M-delete] [S-delete] [C-M-delete]
        [C-S-delete] [M-S-delete] [C-M-S-delete]
        [C-deletechar] [M-deletechar]
        [S-deletechar] [C-M-deletechar] [C-S-deletechar]
        [M-S-deletechar] [C-M-S-deletechar]
        [C-up] [C-down] [C-right] [C-left]
        [M-up] [M-down] [M-right] [M-left]
        [S-up] [S-down] [S-right] [S-left]
        [C-M-up] [C-M-down] [C-M-right] [C-M-left]
        [C-S-up] [C-S-down] [C-S-right] [C-S-left]
        [M-S-up] [M-S-down] [M-S-right] [M-S-left]
        [C-M-S-up] [C-M-S-down] [C-M-S-right] [C-M-S-left]
        [C-home] [M-home] [S-home] [C-M-home] [C-S-home]
        [M-S-home] [C-M-S-home]
        [C-end] [M-end] [S-end] [C-M-end] [C-S-end]
        [M-S-end] [C-M-S-end]
        [C-prior] [M-prior] [S-prior] [C-M-prior]
        [C-S-prior] [M-S-prior] [C-M-S-prior]
        [C-next] [M-next] [S-next] [C-M-next] [C-S-next]
        [M-S-next] [C-M-S-next]

        ;; term-keys
        [?\e ?\C-\]]

        ;; quick-window-jump
        [?\e ?o]))
(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(keymap-global-set "C-x p s" #'eat-project)
(after-load! project
  (define-key project-other-window-map "s" #'eat-project-other-window)
  (when (consp project-switch-commands)
    (alist-delq! project-switch-commands project-shell)
    (add-to-list 'project-switch-commands '(eat-project "Eat") t)))

(defvar eat-buffer-name)
(defun +eat/here (&optional arg)
  "Run `eat' in the current directory.
With non-nil prefix-argument ARG, the directory will be read from the
minibuffer."
  (interactive "P") (require 'eat)
  (let ((dir
         (if arg
             (expand-file-name
              (read-directory-name "Run eat in directory:"))
           default-directory)))
    (let
        ((default-directory dir)
         (eat-buffer-name (concat "*" dir " : eat*")))
      (eat nil nil))))

(defvar eat-terminal)
(declare-function eat-term-send-string "eat.el" (terminal string))
(declare-function eat-self-input "eat.el" (n &optional e))
(defun eat-send-pass nil
  "Send a password from Password-Store to the terminal."
  (interactive)
  (if eat-terminal nil
    (user-error "Process not running"))
  (require 'password-store)
  (password-store-get
   (completing-read "Password-store entry: " nil 'require-match t)
   (let ((my-term eat-terminal))
     (lambda (password)
       (eat-term-send-string my-term password)
       (eat-self-input 1 'return)))))

(setopt eat-kill-buffer-on-exit t)

;;;; with-editor

(keymap-global-set "<remap> <async-shell-command>" #'with-editor-async-shell-command)
(keymap-global-set "<remap> <shell-command>" #'with-editor-shell-command)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'term-exec-hook #'with-editor-export-editor)
(add-hook 'vterm-mode-hook #'with-editor-export-editor)

(defun +with-editor--export-editor-to-eat (proc)
  (eval-and-compile (require 'with-editor))
  (if with-editor-emacsclient-executable
      (with-editor
        (with-editor--setup)
        (while (accept-process-output proc 1 nil t))
        (when-let* ((v (getenv "EDITOR")))
          (eat-term-send-string eat-terminal (format " export EDITOR=%S" v))
          (eat-self-input 1 'return))
        (when-let* ((v (getenv "EMACS_SERVER_FILE")))
          (eat-term-send-string eat-terminal (format " export EMACS_SERVER_FILE=%S" v))
          (eat-self-input 1 'return))
        (eat-term-send-string eat-terminal " clear")
        (eat-self-input 1 'return))
    (error "Cannot use sleeping editor in this buffer")))
(add-hook 'eat-exec-hook '+with-editor--export-editor-to-eat)

;;;; pyim

(declare-function pyim-cregexp-build "pyim-cregexp.el" (string &optional char-level-num chinese-only))
(defun +orderless-pinyin (component)
  (require 'pyim)
  (pyim-cregexp-build component 3 t))

(defvar +pyim--corfu nil)
(defun +pyim--activate ()
  (when (boundp 'corfu-auto)
    (set (make-local-variable '+pyim--corfu)
         corfu-auto)
    (set (make-local-variable 'corfu-auto) nil)))
(defun +pyim--deactivate ()
  (if (boundp 'corfu-auto)
      (set (make-local-variable 'corfu-auto)
           +pyim--corfu)))
(add-hook 'pyim-activate-hook '+pyim--activate)
(add-hook 'pyim-deactivate-hook '+pyim--deactivate)

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

;;;; term-keys

(defvar term-keys/prefix)
(setq term-keys/prefix "\033\035")    ; ^[^]

(defun +term-keys--autoload (_prompt)
  (require 'term-keys)
  (let* ((keys (this-command-keys))
         (events (mapcar (lambda (ev) (cons t ev))
                         (listify-key-sequence keys))))
    (setq unread-command-events (append events unread-command-events))
    nil))

(defun +term-keys--tty-setup (&optional terminal)
  ;; TERMINAL: nil means the current terminal
  (when (and (eq (framep-on-display terminal) t)
             (not (bound-and-true-p term-keys-mode)))
    (with-selected-frame (car (frames-on-display-list terminal))
      (define-key input-decode-map term-keys/prefix '+term-keys--autoload))))

(add-hook 'tty-setup-hook '+term-keys--tty-setup)
(mapc '+term-keys--tty-setup (terminal-list))

(after-load! term-keys
  (remove-hook 'tty-setup-hook '+term-keys--tty-setup)
  ;; (fmakunbound '+term-keys--tty-setup)
  ;; (fmakunbound '+term-keys--autoload)
  (dolist (terminal (terminal-list))
    (when (eq (framep-on-display terminal) t)
      (with-selected-frame (car (frames-on-display-list terminal))
        (define-key input-decode-map term-keys/prefix nil))))
  (term-keys-mode)
  (dolist (terminal (terminal-list))
    (when (eq (framep-on-display terminal) t)
      (with-selected-frame (car (frames-on-display-list terminal))
        (term-keys/init)))))

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

;;;; repeat

(defun +repeat--post-command ()
  (when (function-get this-command 'repeat-map)
    (message "Command %S has a `repeat-map'" this-command)
    (require 'repeat)
    (declare-function repeat-post-hook "repeat.el")
    (repeat-post-hook)))
(add-hook 'post-command-hook '+repeat--post-command)
(after-load! repeat
  (remove-hook 'post-command-hook '+repeat--post-command)
  (repeat-mode))

;;;; savehist

(autoload 'savehist-minibuffer-hook "savehist")
(add-hook 'minibuffer-setup-hook #'savehist-minibuffer-hook)

(after-load! (:or savehist compile corfu clipetty)
  (setq savehist-additional-variables '(kill-ring
                                        register-alist
                                        compile-command
                                        corfu-history))
  (savehist-mode))

;;;; auth-sources
(setopt auth-sources '("~/.authinfo.gpg" password-store))
(after-load! auth-source
  (auth-source-pass-enable))

(after-load! password-store
  (add-hook 'savehist-save-hook #'password-store-clear))

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
         tabulated-list-mode-hook tar-mode-hook
         telega-root-mode-hook))
(dolist (hook lin-mode-hooks)
  (add-hook hook #'lin-mode))

(declare-function hl-line-highlight "hl-line.el")
(defun +lin-line--next-error-h ()
  (with-current-buffer next-error-buffer
    (save-selected-window
      (when-let* ((win (get-buffer-window (current-buffer))))
        (select-window win)
        (recenter))
      (when (bound-and-true-p lin-mode)
        (hl-line-highlight)))))

(after-load! lin
  (setopt lin-face 'lin-magenta)
  (setopt lin-mode-hooks
          (seq-union lin-mode-hooks (custom--standard-value 'lin-mode-hooks)))
  (lin-global-mode))

(add-hook 'next-error-hook '+lin-line--next-error-h)
(add-hook 'gnus-visual-mark-article-hook #'hl-line-highlight)

;;;; email and gnus

(setq mm-discouraged-alternatives
      (eval-when-compile
        (require 'mm-decode)
        (append '("text/html" "text/richtext")
                (custom--standard-value 'mm-discouraged-alternatives))))

;;;; emacs-server

;; Workaround windows encoding issue
(defun +server--process-filter-coding-system (&rest args)
  (let ((file-name-coding-system locale-coding-system))
    (apply args)))

(after-load! server
  (require 'org-protocol)
  (when (memq system-type '(windows-nt ms-dos))
    (add-hook #'server-process-filter :around '+server--process-filter-coding-system))
  (unless (server-running-p)
    (server-start)))

;;;; epg

(setopt epg-pinentry-mode 'loopback)
(setopt epa-keys-select-method 'minibuffer)

;;;; tui

(autoload 'tui-run "tui" nil t)
(autoload 'tui-rg "tui" nil t)
(autoload 'tui-ugrep "tui" nil t)
(autoload 'tui-yazi "tui" nil t)
(autoload 'tui-kill "tui" nil t)
(autoload 'tui-line "tui" nil t)
(autoload 'tui-find "tui" nil t)
(autoload 'tui-locate "tui" nil t)
(after-load! tui
  (alist-setq! display-buffer-alist "^\\*tui-" '((display-buffer-same-window))))

;;;; gptel

(defvar embark-general-map)
(after-load! embark
  (keymap-set embark-general-map "?" #'gptel-quick))

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

(autoload 'image-slicing-mode "image-slicing.el" nil t)
(add-hook 'eww-mode-hook #'image-slicing-mode)
(add-hook 'discourse-topic-mode-hook #'image-slicing-mode)

;;;; eww

(declare-function eww-current-url "eww.el")

(defun eww+miniflux-trim ()
  (when (string-match-p "^https://miniflux\\." (eww-current-url))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when-let* ((match (text-property-search-forward 'shr-target-id "page-header-title" 'member)))
          (delete-region (point-min) (prop-match-beginning match)))))))

(defun eww+kagi-trim ()
  (when (string-match-p "^https://kagi\\.com" (eww-current-url))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when-let* ((match (text-property-search-forward 'shr-target-id "tonav" #'member)))
          (delete-region (prop-match-beginning match) (prop-match-end match)))))))

(defvar bookmark-current-bookmark)
(declare-function bookmark-location "bookmark.el" (arg1))

(defun eww-reset-current-bookmark ()
  (when (and bookmark-current-bookmark
             (not (equal (eww-current-url) (bookmark-location bookmark-current-bookmark))))
    (setq bookmark-current-bookmark nil)))

(add-hook 'eww-after-render-hook 'eww+miniflux-trim)
(add-hook 'eww-after-render-hook 'eww+kagi-trim)
(after-load! bookmark
  (add-hook 'eww-after-render-hook 'eww-reset-current-bookmark))

;;;; bookmark

(defvar pp-default-function)
(defun +bookmark--pp-28 (&rest args)
  (let ((pp-default-function 'pp-28))
    (apply args)))

(autoload 'url-bookmark-add "bookmark-extras.el" "" t)
(keymap-global-set "C-x r u" #'url-bookmark-add)
(after-load! bookmark
  (advice-add #'bookmark-write-file :around '+bookmark--pp-28)

  (setq bookmark-save-flag 1
        bookmark-watch-bookmark-file 'silent
        bookmark-version-control t)

  (require 'bookmark-extras))

;;;; org

(autoload 'org-store-link "ol.el" nil t)

(defun +org/toggle-emphasis-markers ()
  "Toggle the display of emphasis markers."
  (interactive)
  (defvar org-hide-emphasis-markers)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-flush))

(after-load! org
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (define-keymap :keymap org-mode-map
    "C-c o M" #'+org/toggle-emphasis-markers
    "C-c o m" #'org-modern-mode)
  (setopt org-export-backends '(html latex texinfo))
  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c"))))

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

;;;; discourse

;; Currently discourse-emacs doesn't have any autoload cookies, so I
;; added them myself.
(autoload 'discourse-login "discourse.el" nil t)
(after-load! discourse
  (setq discourse-debug nil)
  (keymap-set mode-specific-map "d" #'doc-map)
  (define-key doc-map "t" 'discourse-get-latest-topics)
  (define-key doc-map "c" 'discourse-get-categories)
  (define-key doc-map "l" 'discourse-login))

;;;; copilot

(after-load! copilot
  ;; TODO choose better keybindings
  (define-keymap :keymap copilot-mode-map
 "<tab>" #'copilot-accept-completion
    "C-<tab>" #'copilot-accept-completion-by-word))

;;;; browser-hist

(keymap-global-set "M-s b" #'browser-hist-search)
(after-load! browser-hist
  (alist-setq! browser-hist-db-paths zen
	      (cond
	       ((memq system-type
		      '(cygwin windows-nt ms-dos))
	        "$APPDATA/zen/Profiles/*/places.sqlite")))
  (alist-setq! browser-hist--db-fields
    zen '("title" "url" "moz_places" "ORDER BY last_visit_date desc"))
  (setq browser-hist-default-browser 'zen))

;;;; vundo

(after-load! vundo
  (setopt vundo-glyph-alist vundo-unicode-symbols))

;;;; markdown

(after-load! markdown-mode
  (keymap-set markdown-mode-map "C-x C-q" #'markdown-view-mode)
  (keymap-set markdown-view-mode-map "C-x C-q" #'markdown-mode)
  (keymap-set gfm-mode-map "C-x C-q" #'gfm-view-mode)
  (keymap-set gfm-view-mode-map "C-x C-q" #'gfm-mode))

;;;; p-search

(after-load! p-search
  (require 'psx-info))
(autoload 'p-search "p-search" nil t)

;;;; copyright

(setopt copyright-year-ranges t)
(add-hook 'before-save-hook #'copyright-update)

;;;; sftp
(autoload 'sftp "sftp" nil t)

;;;; buffer terminator

(after-load! buffer-terminator
  (setq buffer-terminator-verbose nil))

(unless (bound-and-true-p buffer-terminator-mode)
  (run-with-idle-timer 600 nil #'buffer-terminator-mode))


;;;; uptime

(defun uptime-notify ()
  (message "Emacs has been running for %s" (emacs-uptime)))

(defvar uptime-notification-timer
  (run-with-timer 1800 1800 #'uptime-notify))


;;;; display buffer alist


(defun find-early-init-file ()
  "Find `early-init-file'."
  (interactive)
  (find-file early-init-file))

(defun find-user-init-file ()
  "Find `user-init-file'."
  (interactive)
  (find-file user-init-file))

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
  :doc "Keymap for calling external tools."
  :prefix 'tool-map
  :name "Tool map"
  "A" #'gptel
  "a" #'gptel-send
  "t" #'tui-run
  "r" #'tui-rg
  "g" #'tui-ugrep
  "y" #'tui-yazi
  "k" #'tui-kill
  "l" #'tui-line
  "f" #'tui-find
  "d" #'tui-locate
  "e" #'+eshell/here
  "s" #'+eat/here
  "c" #'ctags-menu)

(defvar-keymap doc-map
  :doc "Documentation commands."
  :prefix 'doc-map
  :name "Doc map"
  "d" #'devdocs-lookup
  "i" #'devdocs-install
  "p" #'devdocs-peruse
  "r" #'rust-docs-lookup
  "g" #'good-doc-lookup
  "l" #'discourse-login)

(defvar-keymap file-map
  :doc "Open file commands."
  :prefix 'file-map
  :name "File map"
  "e" #'find-early-init-file
  "i" #'find-user-init-file
  "a" #'ffap
  "r" #'ff-find-related-file
  "R" #'recentf-open
  "n" #'rename-visited-file
  "b" #'backup-list-backups)

(defvar-keymap toggle-map
  :doc "Keymap for toggling options."
  :prefix 'toggle-map
  :name "Toggle map"
  "f" #'display-fill-column-indicator-mode
  "l" #'display-line-numbers-mode
  "o" #'outline-minor-mode
  "x" #'+toggle-transparent
  "v" #'visual-line-mode)

(defvar-keymap debug-map
  :doc "Keymap for debugging commands."
  :prefix 'debug-map
  :name "Debug map"
  "e" #'toggle-debug-on-error
  "q" #'toggle-debug-on-quit
  "f" #'debug-on-entry
  "v" #'debug-on-variable-change
  "c f" #'cancel-debug-on-entry
  "c v" #'cancel-debug-on-variable-change)

(define-keymap :keymap mode-specific-map
  "A" #'org-agenda
  "C" #'org-capture
  "D" debug-map
  "L" #'org-store-link
  "S" straight-prefix-map
  "G" #'gnus
  "T" toggle-map
  "V" #'vundo
  "P" #'p-search
  "a" #'embark-act
  "t" tool-map
  "d" doc-map
  "e" #'cape-prefix-map
  "f" file-map
  "o" toggle-map
  "M-g" #'magit-file-dispatch
  "p" #'project-prefix-map
  "s" #'deadgrep
  "v" #'vc-prefix-map
  "w" #'window-prefix-map)

(define-keymap :keymap ctl-x-map
  "C-c" #'restart-emacs
  "k" #'kill-current-buffer
  "g" #'magit-status-quick
  "M-g" #'magit-dispatch)

(define-keymap :keymap global-map
  "M-c" #'capitalize-dwim
  "M-l" #'downcase-dwim
  "M-u" #'upcase-dwim)


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
