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

(if (featurep 'init)
    (load early-init-file)
  (require 'early-init early-init-file t))

;;;; pre-init

(defvar pre-init-file (locate-user-emacs-file "pre-init.el")
  "The file to load before the init file.")

(when (file-exists-p pre-init-file)
  (let ((straight-current-profile 'user))
    (load pre-init-file nil t)))

(setq straight-current-profile nil)


;;;; keymap

(define-prefix-command 'tool-map 'tool-map "Tool")
(keymap-set mode-specific-map "t" 'tool-map)

(define-prefix-command 'doc-map 'doc-map "Doc")
(keymap-set mode-specific-map "d" 'doc-map )

(defalias 'search-map search-map)
(keymap-global-set "M-s" 'search-map)

(defalias 'goto-map goto-map)
(keymap-global-set "M-g" 'goto-map)

 
;;;; meow-edit

(straight-use-package 'meow)

(setup meow
  (require 'meow)
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
  (meow-motion-overwrite-define-key
   '("<escape>" . ignore))
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
   '("}" . meow-forward-barf))
  (meow-global-mode))

(straight-use-package 'meow-tree-sitter)

(setup meow-tree-sitter
  (meow-tree-sitter-register-defaults))


;;;; straight

(setup straight
  (define-prefix-command 'straight-prefix-map nil "Straight")
  (:global "C-c S" straight-prefix-map
           "C-c S c" straight-check-package
           "C-c S C" straight-check-all
           "C-c S p" straight-pull-package
           "C-c S P" straight-pull-all
           "C-c S f" straight-fetch-package
           "C-c S F" straight-fetch-all
           "C-c S b" straight-rebuild-package
           "C-c S B" straight-rebuild-all
           "C-c S v" straight-freeze-versions
           "C-c S V" straight-thaw-versions
           "C-c S u" straight-use-package
           "C-c S d" straight-visit-package
           "C-c S w" straight-visit-package-website
           "C-c S g" +straight/visit-package-repository))

(defun +straight/visit-package-repository (pkg)
  (interactive (list (straight--select-package "Visit: ")))
  (let ((repo (plist-get (gethash pkg straight--repo-cache)
                        :local-repo)))
    (magit-status-setup-buffer (straight--repos-dir repo))))

(setup straight-x
  (define-prefix-command 'straight-x-prefix-map nil "Straight-X")
  (:global "C-c S x" straight-x-prefix-map)
  (:with-function straight-x-fetch-all
    (:autoload-this nil t)
    (:bind-to "C-c S x f")))


;;;; faces

(setup faces
  (with-demoted-errors "Failed to setup fonts for Chinese characters: %S"
    (set-fontset-font (frame-parameter nil 'font) 'han "Sarasa Gothic CL")
    (set-fontset-font (frame-parameter nil 'font) 'cjk-misc "Sarasa Gothic CL"))
  (set-fontset-font t 'han "Sarasa Gothic CL")
  (set-face-attribute 'default nil :family "Iosevka SS04")
  (set-face-attribute 'fixed-pitch nil :family "Iosevka SS04")
  (set-face-attribute 'variable-pitch nil :family "Sarasa UI CL")

  (add-to-list 'face-font-family-alternatives '("Sarasa Gothic CL" "Iosevka SS04"))
  (add-to-list 'face-font-family-alternatives '("Sarasa UI CL" "Sarasa Gothic CL" "Iosevka SS04"))
  (setopt face-font-family-alternatives (append face-font-family-alternatives nil)))

;;;; delight

(straight-use-package 'delight)

(setup delight
  (setup-define :delight
    (lambda (&optional spec value)
      `(delight ',(or spec (setup-get 'mode)) ,value t))
    :after-loaded t
    :documentation "Hide the mode lighter."))

;;;; nerd-icons

(straight-use-package 'nerd-icons)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'nerd-icons-completion)
(straight-use-package 'nerd-icons-ibuffer)
(straight-use-package 'nerd-icons-dired)

(setup nerd-icons
  (setup nerd-icons-corfu
    (add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  (setup nerd-icons-completion
    (:hook-into marginalia-mode))
  (setup nerd-icons-ibuffer
    (:hook-into ibuffer-mode))
  (setup nerd-icons-dired
    (:delight)
    (:hook-into dired-mode))
  (:only-if (display-graphic-p))
  (require 'nerd-icons)
  (nerd-icons-set-font))

;;;; pixel-scroll

(setup pixel-scroll
  (:with-function (pixel-scroll-up pixel-scroll-down)
    (:autoload-this))
  (setq mwheel-scroll-up-function #'pixel-scroll-up
        mwheel-scroll-down-function #'pixel-scroll-down)
  (:when-loaded
    (pixel-scroll-mode)))

;;;; window

(setup window
  ;; TODO: maybe this prefix-map is not useful enough to be bound
  ;; under `mode-specific-map'
  (fset 'window-prefix-map window-prefix-map)
  (:global "C-c w" window-prefix-map)
  (setq kill-buffer-quit-windows t
        quit-restore-window-no-switch t))

;;;; help

(setup help
  (:when-loaded
    (setq help-enable-variable-value-editing t
          help-enable-completion-autoload nil)))

;;;; backup

(setup backup
  (:with-function (list-backups backup-list-backups)
    (:autoload-this nil t))
  (:global "C-c B" list-backups))

;;;; shortdoc

(setup shortdoc
  (when (fboundp 'shortdoc-help-fns-examples-function)
    (add-hook 'help-fns-describe-function-functions
              #'shortdoc-help-fns-examples-function 50)))

;;;; breadcrumb

(straight-use-package 'breadcrumb)

(setup breadcrumb
  (defun +breadcrumb--prog-mode ()
    (setq-local header-line-format '((:eval (breadcrumb-project-crumbs))
                                     ": "
                                     (:eval (breadcrumb-imenu-crumbs)))))
  (defun +breadcrumb--text-mode ()
    (setq-local header-line-format '((:eval (breadcrumb-imenu-crumbs)))))

  (add-hook 'text-mode-hook #'+breadcrumb--text-mode)
  (add-hook 'conf-mode-hook #'+breadcrumb--text-mode)
  (add-hook 'prog-mode-hook #'+breadcrumb--prog-mode))

;;;; vertico

(straight-use-package 'vertico)

(setup vertico
  (:also-load orderless)
  (:with-function vertico--advice
    (:autoload-this)
    (advice-add #'completing-read-default :around #'vertico--advice)
    (advice-add #'completing-read-multiple :around #'vertico--advice))
  (:when-loaded
    (setq enable-recursive-minibuffers t)
    (vertico-mode)
    (:global "M-R" vertico-repeat)
    (:with-map vertico-map
      (:bind "M-P" vertico-repeat-previous
             "M-N" vertico-repeat-next))
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (:with-map vertico-map
      (:bind "RET" vertico-directory-enter
             "DEL" vertico-directory-delete-char
             "M-DEL" vertico-directory-delete-word))
    (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)
    (:with-map vertico-map
      (:bind "C-q" vertico-quick-insert
             "M-q" vertico-quick-exit))
    (:when-loaded
      (setq vertico-quick1 "aoeuip"
            vertico-quick2 "dhtnslm"))))

;;;; marginalia

(straight-use-package 'marginalia)

(setup marginalia
  (:with-function marginalia--minibuffer-setup
    (:autoload-this)
    (:hook-into minibuffer-setup-hook))
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)
  (:when-loaded
    (add-to-list 'marginalia-prompt-categories
                 '("\\<info manuals\\>" . info-manual))
    (add-to-list 'marginalia-prompt-categories
                 '("\\<manual name\\>" . info-manual))
    (marginalia-mode)))

;;;; crm

(defvar crm-separator)

(setup crm
  (:when-loaded
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
            (cdr args)))))

;;;; cursor-sensor

(setup cursor-sensor
  (:with-mode cursor-intangible-mode
    (:hook-into minibuffer-setup-hook))
  (:when-loaded
    (setq minibuffer-prompt-properties '(read-only t face minibuffer-prompt cursor-intangible t))))

;;;; pulsar

(straight-use-package 'pulsar)

(setup pulsar
  (pulsar-global-mode)
  (setq pulse-flag t)
  (add-to-list 'pulsar-pulse-functions #'ace-window))

;;;; goggles

(straight-use-package 'goggles)

(setup goggles
  (:delight)
  (:hook-into prog-mode text-mode))

;;;; orderless

(straight-use-package 'orderless)

(setup orderless
  (:when-loaded
    (setq completion-styles '(orderless basic))
    (orderless-define-completion-style orderless+flex
      (orderless-matching-styles '(orderless-flex)))
    (orderless-define-completion-style orderless+initialism
      (orderless-matching-styles '(orderless-initialism
                                   orderless-literal
                                   orderless-regexp)))
    (setq completion-category-overrides
          '((file . ((styles . (basic partial-completion))))
            (symbol . ((styles . (orderless+flex))))
            (symbol-help . ((styles . (orderless+flex))))
            (command . ((styles . (orderless+initialism))))
            (variable . ((styles . (orderless+initialism))))
            (eglot . ((styles . (orderless))))
            (eglot-capf . ((styles . (orderless))))))

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

    (add-to-list 'orderless-style-dispatchers #'+orderless--consult-dispatch)))

;;;; corfu

(straight-use-package 'corfu)
(straight-use-package 'corfu-terminal)

(setup corfu
  (:also-load orderless)
  (define-advice completion-in-region (:before (&rest _) corfu)
    (require 'corfu))
  (:when-loaded
    (advice-remove 'completion-in-region #'completion-in-region@corfu)
    (setq completion-cycle-threshold 0
          tab-always-indent 'complete
          text-mode-ispell-word-completion nil
          read-extended-command-predicate #'command-completion-default-include-p)
    (setq corfu-cycle t
          corfu-preselect 'prompt)
    (setq corfu-quick1 "aoeuip"
          corfu-quick2 "dhtnslm")
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode)
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history))

    (:with-map corfu-map
      (:bind "C-q" corfu-quick-insert
             "M-q" corfu-quick-complete
             "TAB" corfu-next
             "S-TAB" corfu-previous
             "<backtab>" corfu-previous))))

(setup corfu-terminal
  (:only-if (not (featurep 'tty-child-frames)))
  (:hook-into tty-setup-hook)
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

;;;; cape

(straight-use-package 'cape)

(setup cape
  (:global "C-c e" cape-prefix-map)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;; dabbrev

(setup dabbrev
  (:when-loaded
    (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)))

;;;; abbrev

(setup abbrev
  :delight)

;;;; tempel

(straight-use-package 'tempel)

(setup tempel
  (:global "M-+" tempel-complete
           "M-*" tempel-insert)
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (:when-loaded
    (setq tempel-path (concat user-emacs-directory "/templates/*.eld"))
    (defun tempel-include (elt)
      (when (eq (car-safe elt) 'i)
        (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
            (cons 'l template)
          (message "Template %s not found" (cadr elt))
          nil)))
    (add-to-list 'tempel-user-elements #'tempel-include)))

;;;; embark

(straight-use-package 'xterm-color)

(straight-use-package 'embark)

(setup embark
  (:global "C-." embark-act
           "C-c a" embark-act)
  (setq prefix-help-command #'embark-prefix-help-command)
  (:when-loaded
    (setq embark-indicators '(embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))

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
        (let ((ansi-color-apply-face-function
               (if use-overlays #'ansi-color-apply-overlay-face
                 #'ansi-color-apply-text-property-face)))
          (ansi-color-apply-on-region beg end))))

    (keymap-set embark-file-map "#" '+embark/find-file-as-root)
    (keymap-set embark-bookmark-map "W" '+embark/eww-open-bookmark)
    (keymap-set embark-bookmark-map "u" '+embark/browse-url-open-bookmark)
    (keymap-set embark-region-map "[" '+embark/apply-ansi-color)))

;;;; consult

(straight-use-package 'pcre2el)

(setup consult
  (:global "C-c M-x" consult-mode-command
           "C-c h" consult-history
           "C-c k" consult-kmacro
           "C-c m" consult-man
           "C-c i" consult-info
           [remap Info-search] consult-info
           ;; C-x bindings in `ctl-x-map'
           "C-x M-:" consult-complex-command ;; orig. repeat-complex-command
           "C-x b" consult-buffer ;; orig. switch-to-buffer
           "C-c b" consult-buffer
           "C-x 4 b" consult-buffer-other-window ;; orig. switch-to-buffer-other-window
           "C-x 5 b" consult-buffer-other-frame ;; orig. switch-to-buffer-other-frame
           "C-x t b" consult-buffer-other-tab ;; orig. switch-to-buffer-other-tab
           "C-x r b" consult-bookmark         ;; orig. bookmark-jump
           "C-x p b" consult-project-buffer ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           "M-#" consult-register-load
           "M-'" consult-register-store ;; orig. abbrev-prefix-mark (unrelated)
           "C-M-#" consult-register
           ;; Other custom bindings
           "M-y" consult-yank-pop ;; orig. yank-pop
           ;; M-g bindings in `goto-map'
           "M-g e" consult-compile-error
           "M-g f" consult-flymake ;; Alternative: consult-flycheck
           "M-g g" consult-goto-line   ;; orig. goto-line
           "M-g M-g" consult-goto-line ;; orig. goto-line
           "M-g o" consult-outline ;; Alternative: consult-org-heading
           "M-g m" consult-mark
           "M-g k" consult-global-mark
           "M-g i" consult-imenu
           "M-g I" consult-imenu-multi
           ;; M-s bindings in `search-map'
           "M-s d" consult-find ;; Alternative: consult-fd
           "M-s c" consult-locate
           "M-s g" consult-grep
           "M-s G" consult-git-grep
           "M-s r" consult-ripgrep
           "M-s l" consult-line
           "M-s L" consult-line-multi
           "M-s k" consult-keep-lines
           "M-s u" consult-focus-lines
           ;; Isearch integration
           "M-s e" consult-isearch-history)

  (:with-map isearch-mode-map
    (:bind "M-e" consult-isearch-history
           "M-s e" consult-isearch-history
           "M-s l" consult-line
           "M-s L" consult-line-multi))

  (:with-map minibuffer-local-map
    (:bind "M-s" consult-history
           "M-r" consult-history))

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (defun +consult--read-file-name-function (prompt &optional dir _default mustmatch initial pred)
    (let* ((default-directory (abbreviate-file-name (or dir default-directory)))
           (minibuffer-completing-file-name t)
           (pred (or pred 'file-exists-p)))
      (require 'consult)
      ;; Use consult--read-1 instead of consult--read to suppress consult customizations.
      ;; TODO figure out if this is the correct way
      ;; should I bind this-command temporarily?
      (consult--read-1 #'read-file-name-internal
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

  (defun +consult--read-buffer-function (prompt &optional def mustmatch pred)
    (require 'consult)
    ;; Use consult--read-1 instead of consult--read to suppress consult customizations.
    ;; TODO figure out if this is the correct way
    ;; should I bind this-command temporarily?
    (consult--read-1 #'internal-complete-buffer
                     :state (consult--buffer-preview)
                     :default def
                     :prompt (format-prompt (replace-regexp-in-string ":[[:space:]]*\\'" "" prompt) def)
                     :require-match mustmatch
                     :predicate pred
                     :preview-key consult-preview-key
                     :sort t
                     :lookup (lambda (selected &rest _) selected)))

  (setq read-buffer-function #'+consult--read-buffer-function)

  (:when-loaded

    (setq consult-preview-key "M-.")

    (consult-customize
     consult-xref consult-ripgrep consult-grep consult-git-grep
     consult-line consult-focus-lines consult-keep-lines
     consult-imenu
     :preview-key '(:debounce 0.2 any))

    (setq consult-narrow-key "<") ;; "C-+"

    (defun +consult--orderless-regexp-compiler (input type &rest _config)
      (setq input (cdr (orderless-compile input)))
      (cons
       (mapcar (lambda (r) (consult--convert-regexp r type)) input)
       (lambda (str)
         (let ((orderless-match-faces orderless-match-faces))
           (setq orderless-match-faces (vconcat '(consult-highlight-match) orderless-match-faces))
           (orderless--highlight input t str)))))

    (setq-default consult--regexp-compiler #'+consult--orderless-regexp-compiler)

    (cl-pushnew #'url-bookmark-jump (cddr (assoc ?w consult-bookmark-narrow)))))



(straight-use-package 'embark-consult)

(setup embark-consult
  (:also-load grep)
  (:when-loaded
    (when (fboundp 'grep--heading-filter)
      (progn
        (defun +embark-consult-export-grep--headings (&rest _)
          (save-excursion
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (grep--heading-filter))))
        (advice-add #'embark-consult-export-grep :after #'+embark-consult-export-grep--headings)))))


(straight-use-package 'consult-dir)

(setup consult-dir
  (:global "C-x C-d" consult-dir)
  (:with-map minibuffer-local-map
    (:bind "C-x C-d" consult-dir
           "C-x C-j" consult-dir-jump-file)))


(straight-use-package '(consult-everything
                        :host github
                        :repo "jthaman/consult-everything"))

;;;; windmove

(setup windmove
  (defun +windmove--autoload ()
    (interactive)
    (require 'windmove)
    (let ((events (mapcar (lambda (ev) (cons t ev))
                          (listify-key-sequence (this-command-keys)))))
      (setq unread-command-events (append events unread-command-events))))
  (:global "S-<left>" +windmove--autoload
           "S-<right>" +windmove--autoload
           "S-<up>" +windmove--autoload
           "S-<down>" +windmove--autoload)
  (:when-loaded
    (windmove-default-keybindings)))

;;;; popper

(straight-use-package 'popper)

(setup popper
  (:with-function popper-toggle-type
    (:autoload-this nil t))
  (:global "C-`" popper-toggle
           "M-`" popper-cycle
           "C-M-`" popper-toggle-type)
  (setq popper-reference-buffers '("\\*Messages\\*" "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   "-eat\\*$"
                                   "-vterm\\*$"
                                   ;; "\\*Warnings\\*"
                                   "\\*Compile-Log\\*"
                                   "\\*vc-git : "
                                   ;;xref--xref-buffer-mode
                                   help-mode compilation-mode
                                   flymake-diagnostics-buffer-mode))
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
  (defun +popper--display (buffer alist)
    (funcall popper-display-function buffer alist))

  (add-to-list 'display-buffer-alist '(+popper--popup-p (+popper--display)))

  (:when-loaded
    (setq display-buffer-alist (assq-delete-all '+popper--popup-p display-buffer-alist))

    ;; delay select-window to post-command-hook
    (defvar +popper--delayed-window nil)
    (defun +popper--select-delayed-window ()
      (when +popper--delayed-window
        (select-window +popper--delayed-window)
        (setq +popper--delayed-window nil)))
    (add-hook 'post-command-hook '+popper--select-delayed-window 90)
    (defun +popper--select-popup-delayed (buf alist)
      (setq +popper--delayed-window (popper-display-popup-at-bottom buf alist)))
    (setq popper-display-function '+popper--select-popup-delayed)
    (with-eval-after-load 'project
      (setq popper-group-function #'popper-group-by-project))
    (popper-mode)
    (popper-echo-mode)))

;;;; ibuffer

(setup ibuffer
  (:global "<remap> <list-buffers>" ibuffer))

;;;; apheleia

(straight-use-package 'apheleia)

(setup apheleia
  (:global "C-x x /" apheleia-format-buffer
           "C-c C-/" apheleia-format-buffer))

;;;; ws-butler

(straight-use-package 'ws-butler)

(setup ws-butler
  (:delight)
  (:hook-into find-file-hook))

;;;; whitespace

(setup whitespace
  (:delight)
  (:hook-into prog-mode conf-mode yaml-mode)
  (:when-loaded
    (setq whitespace-style '(face trailing empty indentation space-before-tab space-after-tab))))

;;;; recentf

(setup recentf
  (:with-function recentf-track-opened-file
    (:autoload-this)
    (:hook-into file-file-hook))
  (with-eval-after-load 'consult
    (setf (plist-get consult--source-recent-file :enabled)
          (lambda ()
            (require 'recentf)
            recentf-mode)))
  (setq recentf-max-saved-items 128)
  (:when-loaded
    (let ((inhibit-message t))
      (recentf-mode))))

;;;; saveplace

(setup saveplace
  (:with-function (save-place-find-file-hook save-place-dired-hook)
    (:autoload-this))
  (add-hook 'find-file-hook #'save-place-find-file-hook)
  (add-hook 'dired-initial-position-hook #'save-place-dired-hook)
  (:when-loaded
    (save-place-mode)))

;;;; autorevert

(setup autorevert
  (:with-function auto-revert--global-adopt-current-buffer
    (:autoload-this)
    (:hook-into find-file-hook))
  (setq auto-revert-remote-files t
        auto-revert-avoid-polling t)
  (:when-loaded
    (global-auto-revert-mode)))

;;;; dired

(setup dired
  (:hook dired-omit-mode)
  (setq dired-listing-switches "-lah"
        dired-hide-details-hide-absolute-location t
        dired-x-hands-off-my-keys nil))

;;;; compile

(setup compile
  (setq compilation-always-kill t
        compilation-ask-about-save t
        compilation-scroll-output 'first-error)

  (:when-loaded
    (defun +compile--use-pipe ()
      (setq-local process-connection-type nil))

    (add-hook 'compilation-mode-hook #'+compile--use-pipe)))

;;;; comint

(setup comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048))

;;;; eshell

(setup eshell
  (:when-loaded
    (setq eshell-scroll-to-bottom-on-input t
          eshell-history-size 8192
          eshell-save-history-on-exit t)
    (add-to-list 'eshell-modules-list 'eshell-tramp)
    (defun +eshell--capf ()
      (when (fboundp 'cape-history)
        (add-hook 'completion-at-point-functions #'cape-history 50 t)))
    (add-hook 'eshell-mode-hook '+eshell--capf)
    (with-eval-after-load 'esh-hist
      (when (fboundp 'consult-history)
        (bind-key "M-r" #'consult-history eshell-hist-mode-map)))))

;;;; text-mode

(setup text-mode
  (defun +text-mode--capf ()
    (when (fboundp 'cape-dict)
      (add-hook 'completion-at-point-functions #'cape-dict 90 t)))
  (add-hook 'text-mode-hook #'+text-mode--capf))

;;;; nxml-mode

(setup nxml-mode
  (:when-loaded
    (defun +nxml-mode--flymake ()
      (when (fboundp 'flymake-xmllint)
        (add-hook 'flymake-diagnostics-functions nil #'flymake-xmllint)))
    (add-hook 'nxml-mode-hook #'+nxml-mode--flymake)))

;;;; outline

(straight-use-package 'outline-minor-faces)

(setup outline-minor-faces
  (:hook-into outline-minor-mode))

;;;; adaptive-wrap or visual-wrap

(straight-use-package 'adaptive-wrap)

(setup adaptive-wrap
  (:only-if (not (fboundp 'visual-wrap-prefix-mode)))
  (:with-mode adaptive-wrap-prefix-mode
    (:hook-into visual-line-mode)))

(setup visual-wrap
  (:only-if (locate-library "visual-wrap"))
  (:with-mode visual-wrap-prefix-mode
    (:hook-into visual-line-mode)))

;;;; hl-todo

(straight-use-package 'hl-todo)

(setup hl-todo
  (:hook-into prog-mode conf-mode)
  (:global "C-c t [" hl-todo-previous
           "C-c t ]" hl-todo-next
           "C-c t o" hl-todo-occur
           "C-c t i" hl-todo-insert)
  (:when-loaded
    (let ((map (make-sparse-keymap)))
      (keymap-set map "[" #'hl-todo-previous)
      (keymap-set map "]" #'hl-todo-next)
      (keymap-set map "o" #'hl-todo-occur)
      (put #'hl-todo-previous 'repeat-map map)
      (put #'hl-todo-next 'repeat-map map))))

;;;; display-line-numbers

(setup display-line-numbers
  (:hook-into prog-mode conf-mode)
  (setq display-line-numbers-type 'relative))

;;;; eglot

(straight-use-package 'eglot)

(setup eglot
  (:when-loaded
    (setq eglot-autoshutdown t
          eglot-extend-to-xref t)
    (when (fboundp 'cape-wrap-buster)
      (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
    (when (and (fboundp 'cape-capf-super) (fboundp 'cape-file) (fboundp 'tempel-expand))
      (defun +eglot--capf ()
        (setq-local completion-at-point-functions
                    (list (cape-capf-super #'eglot-completion-at-point
                                           #'tempel-expand
                                           #'cape-file))))
      (add-hook 'eglot-managed-mode-hook #'+eglot--capf))))

;;;; xref

(straight-use-package 'xref)

(setup xref
  (:global "C-<down-mouse-1>" nil
           "C-<C-M-<down-mouse-1>" nil
           "C-<mouse-1>" xref-find-definitions-at-mouse
           "C-M-<mouse-1>" xref-find-references-at-mouse)
  (:when-loaded
    (defvar +xref--max-definitions-in-buffer 5)

    (defun +xref--show-definition (fetcher alist)
      "Use `xref-show-definitions-buffer' if the candidates are few.
Otherwise use `consult-xref'."
      (let ((xrefs (funcall fetcher)))
        (if (length< xrefs +xref--max-definitions-in-buffer)
            (xref-show-definitions-buffer-at-bottom fetcher alist)
          (consult-xref fetcher alist))))

    (setq xref-search-program
          (cond ((executable-find "rg") 'ripgrep)
                ((executable-find "ugrep") 'ugrep)
                (t 'grep))
          xref-show-definitions-function #'+xref--show-definition
          xref-auto-jump-to-first-definition t)))

;;;; ctags

(straight-use-package 'transient)       ; for ctags-menu

(setup ctags-menu
  (:with-function ctags-menu
    (:autoload-this nil t)
    (:bind-to "C-c t m")))

(setup ctags-xref
  (:with-function ctags-xref-backend
    (:autoload-this)
    (add-hook 'xref-backend-functions #'ctags-xref-backend))
  (:when-loaded
    ;; Override `xref-backend-references' for ctags: if GRTAGS exists,
    ;; find reference using `gtags' backend.
    (cl-defmethod xref-backend-references :around ((_backend (eql 'ctags)) identifier)
      (require 'gtags)
      (if (and (locate-dominating-file default-directory "GRTAGS")
               (executable-find "global" t))
          (xref-backend-references 'gtags identifier)
        (cl-call-next-method)))

    (with-eval-after-load 'cc-mode
      (require 'ctags-xref-c))))

;;;; gtags

(setup gtags
  (:with-function (gtags-update gtags-single-update)
    (:autoload-this nil t))
  (:global "C-c t G" gtags-update)
  (add-hook 'after-save-hook #'gtags-single-update))

;;;; devdocs

(straight-use-package 'devdocs)

(setup devdocs
  (:global "C-c d d" devdocs-lookup
           "C-c d i" devdocs-install
           "C-c d p" devdocs-peruse))

;;;; gtkdoc

(setup good-doc
  (:with-function good-doc-lookup
    (:autoload-this nil t)
    (:bind-to "C-c d g")))

;;;; rust-docs

(setup rust-docs
  (:with-function rust-docs-lookup
    (:autoload-this nil t)
    (:bind-to "C-c d r")))

;;;; javascript

(setup js
  (:when-loaded
    (define-advice js-jsx-enable (:after () comments)
      (setq-local comment-region-function #'js-jsx--comment-region))

    (define-advice js-jsx-enable (:after () sgml)
      (require 'sgml-mode)
      (keymap-set-many (current-local-map)
                       "C-c C-b" sgml-skip-tag-backward
                       "C-c C-d" sgml-delete-tag
                       "C-c C-e" sgml-close-tag
                       "C-c C-f" sgml-skip-tag-forward
                       "C-c C-o" sgml-tag
                       "C-c C-t" sgml-tag))))

;;;; lisp-mode

(straight-use-package 'paren-face)

(setup lisp-mode
  (:with-mode lisp-data-mode
    (:hook paren-face-mode)))

;;;; sly

(straight-use-package 'sly)

;;;; geiser

(straight-use-package 'geiser)
(straight-use-package 'geiser-chez)
(straight-use-package 'geiser-chicken)
(straight-use-package 'geiser-guile)

;;;; paren-face

(setup paren-face
  (:when-loaded
    (defun +paren-face--update-color ()
      (set-face-foreground 'parenthesis (modus-themes-get-color-value 'border)))
    (+paren-face--update-color)
    (add-hook 'modus-themes-after-load-theme-hook '+paren-face--update-color)))

;;;; puni

(straight-use-package 'puni)
(setup puni
  (:hook-into prog-mode conf-mode)
  (:hook electric-pair-local-mode)
  (:bind "C-)" puni-slurp-forward
         "C-(" puni-slurp-backward
         "C-}" puni-barf-forward
         "C-{" puni-barf-backward))

;;;; elisp-mode

(setup elisp-mode
  (:with-mode emacs-lisp-mode
    (:and (native-comp-available-p)
          (:bind "C-c C-l" emacs-lisp-native-compile-and-load)))
  (:with-mode lisp-interaction-mode
    (:bind "C-c C-j" eval-print-last-sexp))
  (defun +elisp-mode--setup ()
    (setq-local elisp-flymake-byte-compile-load-path load-path))
  (add-hook 'emacs-lisp-mode-hook #'+elisp-mode--setup))

;;;; pp

(setup pp
  (:global "M-:" pp-eval-expression))

;;;; pp-posframe
(straight-use-package 'posframe)
(setup pp-posframe
  (:with-function (pp-posframe-eval-last-sexp
                   pp-posframe-compile-defun
                   pp-posframe-macroexpand-last-sexp)
    (:autoload-this nil t))
  (:global "C-x C-e" pp-posframe-eval-last-sexp)
  (:with-map emacs-lisp-mode-map
    (:bind "C-M-x" pp-posframe-compile-defun
           "C-c M-e" pp-posframe-macroexpand-last-sexp))
  (:when-loaded
    (defun +pp-posframe--set-color ()
      (setq pp-posframe-parameters `( :boredr-color ,(modus-themes-get-color-value 'border)
                                      :background-color ,(modus-themes-get-color-value 'bg-dim))))
    (add-hook 'modus-themes-after-load-theme-hook #'+pp-posframe--set-color)
    (+pp-posframe--set-color)))

;;;; find-func

(setup find-func
  (:global "C-x F" find-function
           "C-x V" find-variable
           "C-x K" find-function-on-key
           "C-x L" find-library
           "C-x 4 F" find-function-other-window
           "C-x 4 V" find-variable-other-window
           "C-x 4 K" find-function-on-key-other-window
           "C-x 4 L" find-library-other-window
           "C-x 5 F" find-function-other-frame
           "C-x 5 V" find-variable-other-frame
           "C-x 5 K" find-function-on-key-other-frame
           "C-x 5 L" find-library-other-frame))

;;;; eldoc

(setup eldoc
  (:delight))

;;;; cc-mode

(setup cc-mode
  (:when-loaded
    (setq c-tab-always-indent nil
          c-insert-tab-function #'completion-at-point)
    (defun +cc-mode--hook ()
      (add-hook 'flymake-diagnostics-functions #'flymake-clang-tidy nil t))
    (add-hook 'c-mode-common-hook '+cc-mode--hook)))

(straight-use-package 'vala-mode)

;;;; rust-mode

(straight-use-package 'rust-mode)

(setup rust-mode
  (:when-loaded
    (define-advice rust--compile (:around (&rest args) project-prefix-buffer-name)
      (let ((compilation-buffer-name-function #'project-prefixed-buffer-name))
        (apply args)))))

;;;; ruby

(straight-use-package 'ruby-mode)

;;;; sh-script

(setup sh-script
  (:with-mode sh-mode
    (:hook sh-electric-here-document-mode)
    (:local-hook save-file-hook #'executable-make-buffer-file-executable-if-script-p)))

;;;; project

(setup project
  (fset 'project-prefix-map project-prefix-map)
  (:global "C-c p" project-prefix-map)
  (when (commandp 'project-prefix-or-any-command)
    (setq project-switch-commands 'project-prefix-or-any-command))
  (:when-loaded
    (dolist (file '(".project-root" "configure.ac" "Cargo.toml" "package.json"))
      (add-to-list 'project-vc-extra-root-markers file))

    (defun +project--external-roots ()
      (and-let* ((project (project-current))
                 (root (project-root project))
                 (project-root-file (expand-file-name ".project-root" root))
                 ((file-exists-p project-root-file)))
        (with-work-buffer
          (insert-file-contents project-root-file)
          (let ((default-directory root))
            (mapcar #'expand-file-name
                    (string-lines (buffer-string) t nil))))))

    (setq-default project-vc-external-roots-function #'+project--external-roots)

    (setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)

    (when (and (not (functionp project-switch-commands))
               (consp project-switch-commands))
      (add-to-list 'project-switch-commands '(project-compile "Compile") t))))

;;;; buffer-env

(straight-use-package 'buffer-env)

(setup buffer-env
  (:with-function buffer-env-update
    (:hook-into hack-local-variables-hook comint-mode-hook))
  (:when-loaded
    (add-to-list 'buffer-env-command-alist '("/\\.nvmrc\\'" . "~/.nvm/nvm-exec env -0"))
    (setq buffer-env-script-name '(".envrc" ".nvmrc" ".env"))
    (defun +buffer-env/clear-cache ()
      "Clear buffer-env cache."
      (interactive)
      (clrhash buffer-env--cache))))

;;;; vc

(setup vc
  (:global "C-c v" vc-prefix-map)
  (setq vc-follow-symlinks t)
  (:also-load diff-hl))

(setup vc-svn
  ;; Use pinentry to enter SVN credentials.  A GUI-based pinentry is needed.
  (setq vc-svn-global-switches
        '("--force-interactive"
          "--config-option"
          "config:auth:password-stores=gpg-agent")))

;;;; magit

(straight-use-package 'magit)

(setup magit
  (:global "C-x g" magit-status
           "C-x M-g" magit-dispatch
           "C-c M-g" magit-file-dispatch
           "C-x p m" magit-project-status)
  (with-eval-after-load 'project
    (when (consp project-switch-commands)
      (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))
  (:when-loaded
    (defun +magit--ccdp-no-query ()
      "Avoid query process status on exit."
      (when magit-credential-cache-daemon-process
        (set-process-query-on-exit-flag
         magit-credential-cache-daemon-process nil)))
    (advice-add #'magit-maybe-start-credential-cache-daemon :after '+magit--ccdp-no-query)
    (setq magit-wip-mode-lighter "")
    (magit-wip-mode)))

;;;; diff-hl

(straight-use-package 'diff-hl)

(setup diff-hl
  (:with-function (diff-hl-magit-pre-refresh
                   diff-hl-magit-post-refresh)
    (:autoload-this))
  (add-hook 'tty-setup-hook #'diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (:when-loaded
    (global-diff-hl-mode)))

;;;; eat

(straight-use-package `(eat :files (,@straight-default-files-directive "integration" "term" "terminfo")))

(setup eat
  (:only-if (not (memq system-type '(windows-nt ms-dos))))
  (:with-function (eat-eshell-mode eat-eshell-visual-command-mode)
    (:hook-into eshell-load-hook))
  (:global "C-x p s" eat-project)
  (:with-feature project
    (:with-map project-other-window-map
      (:bind "s" eat-project-other-window))
    (:when-loaded
      (when (consp project-switch-commands)
        (setq project-switch-commands (assq-delete-all 'project-shell project-switch-commands))
        (add-to-list 'project-switch-commands '(eat-project "Eat") t))))
  (defun +eat/here (&optional arg)
    "Run `eat' in the current directory.
With non-nil prefix-argument ARG, the directory will be read from the
minibuffer."
    (interactive "P")
    (require 'eat)
    (let ((dir (if arg
                   (expand-file-name (read-directory-name "Run eat in directory:"))
                 default-directory)))
      (let ((default-directory dir)
            (eat-buffer-name (concat "*" dir " : eat*")))
        (eat nil nil))))
  (keymap-global-set "C-c t s" '+eat/here)
  (:when-loaded
    (setq eat-kill-buffer-on-exit t)
    (defun eat-send-pass ()
      (interactive)
      (unless eat-terminal
        (user-error "Process not running"))
      (require 'password-store)
      (eat-term-send-string
       eat-terminal
       (password-store-get (password-store--completing-read t)))
      (eat-self-input 1 'return))))

;;;; with-editor

(straight-use-package 'with-editor)

(setup with-editor
  (:global "<remap> <async-shell-command>" with-editor-async-shell-command)
  (:global "<remap> <shell-command>" with-editor-shell-command)
  (:with-function with-editor-export-editor
    (:hook-into eshell-mode shell-mode term-exec-hook vterm-mode))
  (defun +with-editor--export-editor-to-eat (proc)
    (require 'with-editor)
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
  (add-hook 'eat-exec-hook '+with-editor--export-editor-to-eat))

;;;; pyim

(straight-use-package 'pyim)
(straight-use-package 'pyim-basedict)

(setup pyim
  (defun +orderless-pinyin (component)
    (require 'pyim)
    (pyim-cregexp-build component 3 t))

  (with-eval-after-load 'orderless
    (add-to-list 'orderless-affix-dispatch-alist
                 '(?` . +orderless-pinyin)))

  ;; ;; Set the default input method of Chinese language environments to `pyim'.
  ;; (set-language-info "Chinese-GB18030" 'input-method "pyim")
  ;; (set-language-info "Chinese-GBK" 'input-method "pyim")
  ;; (set-language-info "Chinese-GB" 'input-method "pyim")

  ;; ;; UTF-8 is preferred on Unix-like systems.
  ;; (set-language-info "UTF-8" 'input-method "pyim")

  (:when-loaded
    (defvar +pyim--corfu nil)
    (defun +pyim--activate ()
      (when (boundp 'corfu-auto)
        (setq-local +pyim--corfu corfu-auto)
        (setq-local corfu-auto nil)))
    (defun +pyim--deactivate ()
      (when (boundp 'corfu-auto)
        (setq-local corfu-auto +pyim--corfu)))
    (add-hook 'pyim-activate-hook '+pyim--activate)
    (add-hook 'pyim-deactivate-hook '+pyim--deactivate)

    (pyim-basedict-enable)))


;;;; rime

(straight-use-package 'rime)

(setup rime
  (setq rime-disable-predicates '(meow-normal-mode-p
                                  meow-keypad-mode-p
                                  meow-motion-mode-p
                                  meow-beacon-mode-p))
  (define-advice toggle-input-method (:before (&rest _) rime)
    (setq default-input-method "rime")))

;;;; kinsoku

(setup kinsoku
  (setq word-wrap-by-category t))

;;;; term-keys

(straight-use-package '(term-keys :host github :repo "CyberShadow/term-keys"))

(setup term-keys
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

  (:when-loaded
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
          (term-keys/init))))

    (with-eval-after-load 'eat
      (unless (member [?\e ?\C-\]] eat-semi-char-non-bound-keys)
        (setopt eat-semi-char-non-bound-keys
                (cons [?\e ?\C-\]] eat-semi-char-non-bound-keys))))))

;;;; xterm

(setup term/xterm
  (setq xterm-set-window-title t))

;;;; xt-mouse

(setup xt-mouse
  (:with-mode xterm-mouse-mode
    (:hook-into tty-setup-hook))
  (when (eq (framep-on-display) t)
    (require 'xt-mouse))
  (:when-loaded
    (xterm-mouse-mode)))

;;;; clipetty

(straight-use-package 'clipetty)

(setup clipetty
  (:delight)
  (:hook-into tty-setup-hook)
  (when (eq (framep-on-display) t)
    (require 'clipetty))
  (:when-loaded
    (global-clipetty-mode)))

;;;; repeat

(setup repeat
  :init
  (defun +repeat--post-command ()
    (when (function-get this-command 'repeat-map)
      (message "Command %S has a `repeat-map'" this-command)
      (require 'repeat)
      (repeat-post-hook)))
  (add-hook 'post-command-hook '+repeat--post-command)
  (:when-loaded
    (remove-hook 'post-command-hook '+repeat--post-command)
    (repeat-mode)))

;;;; savehist

(setup savehist
  (:with-function savehist-minibuffer-hook
    (:autoload-this)
    (:hook-into minibuffer-setup-hook))
  (:when-loaded
    (savehist-mode)))

;;;; auth-sources

(setup auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  (:when-loaded
    (auth-source-pass-enable)))

(straight-use-package 'password-store)

;;;; lin

(straight-use-package 'lin)

(setup lin
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
  (:when-loaded
    (setq lin-face 'lin-magenta)
    (lin-global-mode)

    (defun +lin-line--next-error-h ()
      (with-current-buffer next-error-buffer
        (save-selected-window
          (when-let* ((win (get-buffer-window (current-buffer))))
            (select-window win)
            (recenter))
          (when lin-mode
            (hl-line-highlight)))))
    (add-hook 'next-error-hook '+lin-line--next-error-h)

    (add-hook 'gnus-visual-mark-article-hook #'hl-line-highlight)))

;;;; emacs-server

(setup server
  (:when-loaded
    ;; Workaround windows encoding issue
    (when (memq system-type '(windows-nt ms-dos))
      (defun +server--process-filter-coding-system (&rest args)
        (let ((file-name-coding-system locale-coding-system))
          (apply args)))
      (add-hook #'server-process-filter :around '+server--process-filter-coding-system))

    (unless (server-running-p)
      (server-start)))
  (:also-load org-protocol))

;;;; epg

(setup epg
  (setq epg-pinentry-mode 'loopback))

(setup epa
  (setq epa-keys-select-method 'minibuffer))

;;;; tui

(setup tui
  (:only-if (not (memq system-type '(ms-dos windows-nt))))
  (:with-function ( tui-run tui-rg tui-ugrep tui-yazi tui-kill tui-line
                    tui-find tui-locate)
    (:autoload-this nil t))
  (:global "C-c t t" tui-run
           "C-c t r" tui-rg
           "C-c t g" tui-ugrep
           "C-c t y" tui-yazi
           "C-c t k" tui-kill
           "C-c t l" tui-line
           "C-c t f" tui-find
           "C-c t d" tui-locate)
  (:when-loaded
    (setf (alist-get "^\\*tui-" display-buffer-alist nil nil #'equal)
          '((display-buffer-same-window)))))

;;;; gptel

(straight-use-package 'gptel)
(straight-use-package '(gptel-quick :host github :repo "karthink/gptel-quick"))

(setup gptel
  (:global "C-c g" gptel-send))

(setup gptel-quick
  (with-eval-after-load 'embark
    (bind-key "?" #'gptel-quick embark-general-map)))

;;;; logos

(straight-use-package 'logos)
(straight-use-package 'olivetti)

(setup logos
  (:global "<f8>" logos-focus-mode
           "<remap> <narrow-to-region>" logos-narrow-dwim
           "<remap> <forward-page>" logos-forward-page-dwim
           "<remap> <backward-page>" logos-backward-page-dwim)
  (:with-map logos-focus-mode-map
    (:bind "<left>" logos-backward-page-dwim
           "<right>" logos-forward-page-dwim))
  (:when-loaded
    (setq logos-outlines-are-pages t)

    (setq-default logos-hide-cursor nil
                  logos-hide-mode-line t
                  logos-hide-header-line t
                  logos-hide-buffer-boundaries t
                  logos-hide-fringe t
                  logos-buffer-read-only nil
                  logos-scroll-lock nil
                  logos-olivetti t)

    (defun logos-focus--narrow ()
      (when logos-focus-mode
        (logos--narrow-to-page 0)
        (make-local-variable 'logos--restore)
        (push #'widen logos--restore)))

    (add-hook 'logos-focus-mode-hook #'logos-focus--narrow)

    (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

    (setq logos-outline-regexp-alist
          `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
            (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))))))

;;; eww

(setup eww
  (:when-loaded
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
              (delete-region (prop-match-beginning match) (prop-match-end match)))
            ))))

    (defun eww-reset-current-bookmark ()
      (when (and bookmark-current-bookmark
                 (not (equal (eww-current-url) (bookmark-location bookmark-current-bookmark))))
        (setq bookmark-current-bookmark nil)))

    (add-hook 'eww-after-render-hook 'eww+miniflux-trim)
    (add-hook 'eww-after-render-hook 'eww+kagi-trim)

    (with-eval-after-load 'bookmark
      (add-hook 'eww-after-render-hook 'eww-reset-current-bookmark))))

;;;; bookmark

(defvar pp-default-function)

(setup bookmark
  (:when-loaded
    (setq bookmark-save-flag 1
          bookmark-watch-bookmark-file 'silent
          bookmark-version-control t)
    (defun +bookmark--pp-28 (&rest args)
      (let ((pp-default-function 'pp-28))
        (apply args)))
    (advice-add #'bookmark-write-file :around '+bookmark--pp-28)))

(setup bookmark-extras
  (:with-function url-bookmark-add
    (:autoload-this nil t)
    (:bind-to "C-x r u"))
  (with-eval-after-load 'bookmark
    (require 'bookmark-extras)))

;;;; org

(straight-use-package 'org)
(straight-use-package 'org-modern)

(setup org
  (:global "C-c L" org-store-link)
  (:hook org-modern-mode)

  (defun +org/toggle-emphasis-markers ()
    "Toggle the display of emphasis markers."
    (interactive)
    (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
    (font-lock-ensure))
  (:bind "C-c T M" +org/toggle-emphasis-markers))

(setup org-agenda
  (:global "C-c A" org-agenda)
  (:hook org-modern-agenda)
  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c"))))

(setup org-capture
  (:global "C-c C" org-capture)
  (:when-loaded
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
                            "/Entered on/ %U\n" "\n" "%?")))))

;;;; rcirc

(setup rcirc
  (:when-loaded
    (rcirc-track-minor-mode)))

;;;; erc

(straight-use-package 'erc)

(setup erc
  (:when-loaded
    ;; This enables displaying servers and channels in side windows,
    ;; which can be toggled by C-x w s.
    (setopt erc-modules
	    (seq-union '(sasl nicks bufbar nickbar scrolltobottom track)
		       erc-modules)))

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

  ;; insert a newline when I hit <RET> at the prompt, and prefer
  ;; something more deliberate for actually send messages.
  (:bind "RET" nil
	 "C-c C-c" erc-send-current-line)

  ;; prefer one message line without continuation indicators.
  (:with-feature erc-fill
    (setq erc-fill-function #'erc-fill-wrap
	  erc-fill-static-center 18)
    (:with-mode erc-fill-wrap-mode
      (:bind "C-c =" erc-fill-wrap-nudge)))

  ;; prevent JOINs and PARTs from lighting up the mode-line.
  (:with-feature erc-track
    (:when-loaded
      (setopt erc-track-faces-priority-list (remq 'erc-notice-face
						  erc-track-faces-priority-list)))
    (setq erc-track-priority-faces-only 'all)))

;;;; copilot

(straight-use-package 'copilot)

(setup copilot
  (:bind "<tab>" copilot-accept-completion
         "C-<tab>" copilot-accept-completion-by-word))

;;;; browser-hist

(straight-use-package 'browser-hist)

(setup browser-hist
  (:global "M-s b" browser-hist-search)
  (:when-loaded
    (setf (alist-get 'zen browser-hist-db-paths nil t)
          (cond ((memq system-type '(cygwin windows-nt ms-dos))
                 "$APPDATA/zen/Profiles/*/places.sqlite")))
    (setf (alist-get 'zen browser-hist--db-fields)
          '("title" "url" "moz_places" "ORDER BY last_visit_date desc"))
    (setq browser-hist-default-browser 'zen)))

;;;; vundo

(straight-use-package 'vundo)

(setup vundo
  (:when-loaded
    (setq vundo-glyph-alist vundo-unicode-symbols)))

;;;; debian

(straight-use-package 'debian-el)
(straight-use-package 'dpkg-dev-el)

;;;; markdown

(straight-use-package 'markdown-mode)

(setup markdown-mode
  (:bind "C-x C-q" markdown-view-mode)
  (:with-mode markdown-view-mode
    (:bind "C-x C-q" markdown-mode)))

;;;; other packages

(straight-use-package 'yaml-mode)
(straight-use-package 'toml-mode)
(straight-use-package 'colorful-mode)
(straight-use-package 'autocrypt)
(straight-use-package '(cmake-mode :host github :repo "emacsmirror/cmake-mode" :files ("*.el")))
(straight-use-package 'systemd)
(straight-use-package 'colorful-mode)
(straight-use-package 'show-font)


(defun find-early-init-file ()
  "Find `early-init-file'."
  (interactive)
  (find-file early-init-file))

(defun find-user-init-file ()
  "Find `user-init-file'."
  (interactive)
  (find-file user-init-file))


;;;; post-init

(defvar post-init-file (locate-user-emacs-file "post-init.el"))

(when (file-exists-p post-init-file)
  (let ((straight-current-profile 'user))
    (load post-init-file nil t)))

;;;; _

(provide 'init)

;; Local Variables:
;; eval: (outline-minor-mode)
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
