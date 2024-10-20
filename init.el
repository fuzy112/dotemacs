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

(require 'early-init nil t)

(eval-when-compile
  (require 'use-package))

;;;; pre-init

(defvar pre-init--file (locate-user-emacs-file "pre-init.el")
  "The file to load before the init file.")

(when (file-exists-p pre-init--file)
  (load pre-init--file nil t))


;;;; faces

(use-package faces
  :config
  (ignore-errors
    (set-fontset-font (frame-parameter nil 'font) 'han "Sarasa Gothic CL"))
  (set-fontset-font t 'han "Sarasa Gothic CL")
  (set-face-attribute 'default nil :family "Iosevka SS04")
  (set-face-attribute 'fixed-pitch nil :family "Iosevka SS04")
  (set-face-attribute 'variable-pitch nil :family "Sarasa UI CL"))

;;;; nerd-icons

(use-package nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode)
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :config
  (when (display-graphic-p)
    (nerd-icons-set-font)))

;;;; pixel-scroll

(use-package pixel-scroll
  :autoload
  (pixel-scroll-up
   pixel-scroll-down)
  :init
  (setq mwheel-scroll-up-function #'pixel-scroll-up
        mwheel-scroll-down-function #'pixel-scroll-down)
  :config
  (pixel-scroll-mode))

;;;; window

(use-package window
  :bind
  (:map window-prefix-map
        ("q" . quit-window)))           ; this is default for emacs 30


;;;; help

(use-package help
  :defer t
  :config
  (setq help-enable-variable-value-editing t
        help-enable-completion-autoload nil))

;;;; shortdoc

(use-package shortdoc
  :defer t
  :init
  (when (fboundp 'shortdoc-help-fns-examples-function)
    (add-hook 'help-fns-describe-function-functions
              #'shortdoc-help-fns-examples-function 50)))

;;;; breadcrumb

(use-package breadcrumb
  :defer t
  :init
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

(use-package vertico
  :autoload
  (vertico--advice)
  :init
  (advice-add #'completing-read-default :around #'vertico--advice)
  (advice-add #'completing-read-multiple :around #'vertico--advice)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-q" . vertico-quick-insert)
        ("M-q" . vertico-quick-exit))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (setq enable-recursive-minibuffers t)
  (setq vertico-quick1 "aoeuip"
        vertico-quick2 "dhtnslm")
  (vertico-mode))

;;;; marginalia

(use-package marginalia
  :hook
  (minibuffer-setup . marginalia--minibuffer-setup)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :config
  (add-to-list 'marginalia-prompt-categories
               '("\\<info manuals\\>" . info-manual))
  (add-to-list 'marginalia-prompt-categories
               '("\\<manual name\\>" . info-manual))
  (marginalia-mode))

;;;; crm

(use-package crm
  :defer t
  :config
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

(use-package cursor-sensor
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :config
  (setq minibuffer-prompt-properties '(read-only t face minibuffer-prompt cursor-intangible t)))

;;;; pulsar

(use-package pulsar
  :demand t
  :config
  (pulsar-global-mode)
  (setq pulse-flag t)
  (add-to-list 'pulsar-pulse-functions #'ace-window))

;;;; goggles

(use-package goggles
  :hook
  (prog-mode . goggles-mode)
  (text-mode . goggles-mode))

;;;; orderless

(use-package orderless
  :after (:any vertico corfu consult)
  :config
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
          (variable . ((styles . (orderless+initialism))))))


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

  (add-to-list 'orderless-style-dispatchers #'+orderless--consult-dispatch))

;;;; corfu

(use-package corfu
  :defer 20
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        text-mode-ispell-word-completion nil
        read-extended-command-predicate #'command-completion-default-include-p)
  (defun +corfu--load (&rest _)
    (require 'corfu))
  (advice-add 'completion-in-region :before #'+corfu--load)
  :bind
  (:map corfu-map
        ("C-q" . corfu-quick-insert)
        ("M-q" . corfu-quick-complete))
  :config
  (remove-hook 'completion-in-region #'+corfu--load)
  (setq corfu-quick1 "aoeuip"
        corfu-quick2 "dhtnslm")
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-terminal
  :after corfu
  :hook
  (tty-setup . corfu-terminal-mode)
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

;;;; cape

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  :bind
  ("C-c p" . cape-prefix-map))

;;;; dabbrev

(use-package dabbrev
  :defer t
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;;;; tempel

(use-package tempel
  :functions
  tempel--templates
  :bind
  ("M-+" . tempel-complete)
  ("M-*" . tempel-insert)
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (setq tempel-path (concat user-emacs-directory "/templates/*.eld"))


  (defun tempel-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  (add-to-list 'tempel-user-elements #'tempel-include))

;;;; embark

(use-package embark
  :bind
  ("C-." . embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-mixed-indicator-delay 3)

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

  (bind-key "#" '+embark/find-file-as-root embark-file-map)
  (bind-key "W" '+embark/eww-open-bookmark embark-bookmark-map)
  (bind-key "u" '+embark/browse-url-open-bookmark embark-bookmark-map)
  (bind-key "[" '+embark/apply-ansi-color embark-region-map))

;;;; consult

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-definitions-function #'consult-xref)

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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  :preview-key "M-.")

  (consult-customize
   consult-xref consult-ripgrep consult-grep consult-git-grep
   consult-line consult-focus-lines consult-keep-lines
   consult-imenu
   :preview-key '(:debounce 0.2 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  (defun +consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  (setq-default consult--regexp-compiler #'+consult--orderless-regexp-compiler)

  (cl-pushnew #'url-bookmark-jump (cddr (assoc ?w consult-bookmark-narrow))))


(use-package consult-dir
  :bind
  ("C-x C-d" . consult-dir)
  (:map minibuffer-local-map
        ("C-x C-d" . consult-dir)
        ("C-x C-j" . consult-dir-jump-file)))

;;;; windmove

(use-package windmove
  :init
  (defun +windmove--autoload ()
    (interactive)
    (require 'windmove)
    (let ((events (mapcar (lambda (ev) (cons t ev))
                          (listify-key-sequence (this-command-keys)))))
      (setq unread-command-events (append events unread-command-events))))
  (bind-keys ("S-<left>" . +windmove--autoload)
             ("S-<right>" . +windmove--autoload)
             ("S-<up>" . +windmove--autoload)
             ("S-<down>" . +windmove--autoload))
  :config
  (windmove-default-keybindings))

;;;; popper

(use-package popper
  :bind
  ("C-`" . popper-toggle)
  ("M-`" . popper-cycle)
  ("C-M-`" . popper-toggle-type)
  :defines
  popper-reference-buffers
  popper-display-function
  :init
  (setq popper-reference-buffers '("\\*Messages\\*" "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   "-eat\\*$"
                                   "-vterm\\*$"
                                   "\\*Warnings\\*"
                                   "\\*Compile-Log\\*"
                                   "\\*vc-git : "
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

  (add-to-list 'display-buffer-alist `(+popper--popup-p (+popper--display)))
  :config
  (delq '+popper--puppup-p display-buffer-alist)
  (popper-mode)
  (popper-echo-mode))

;;;; ibuffer

(use-package ibuffer
  :bind
  ("<remap> <list-buffers>" . #'ibuffer))

;;;; avy

(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-timer)
  ("M-g w" . avy-goto-word-1)
  (:map isearch-mode-map
        ("C-'" . avy-isearch))
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?l ?m)))

;;;; ace-window

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?l ?m)))

;;;; apheleia

(use-package apheleia
  :bind
  ("C-x x /" . apheleia-format-buffer)
  :config
  (when (memq system-type '(windows-nt ms-dos))
    (advice-add #'apheleia-format-after-save :override #'ignore)))

;;;; ws-butler

(use-package ws-butler
  :hook
  (find-file . ws-butler-mode))

;;;; activities

(use-package activities
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list))
  :config
  (activities-mode)
  (activities-tabs-mode))

;;;; recentf

(use-package recentf
  :hook
  (find-file . recentf-track-opened-file)
  (kill-buffer . recentf-track-closed-file)
  :init
  (add-hook 'write-file-functions #'recentf-track-opened-file)
  (with-eval-after-load 'consult
    (defvar +consult--source-recentf-file--items
      (plist-get consult--source-recent-file :items))
    (setf (plist-get consult--source-recent-file :enabled) (lambda () t))
    (setf (plist-get consult--source-recent-file :items)
          (lambda ()
            (require 'recentf)
            (unless (bound-and-true-p recentf-mode)
              (recentf-mode))
            (funcall +consult--source-recentf-file--items))))
  :config
  (setq recentf-max-saved-items 128)
  (let ((inhibit-message t))
    (recentf-mode)))

;;;; saveplace

(use-package saveplace
  :hook
  (find-file . save-place-find-file-hook)
  (dired-initial-position . save-place-dired-hook)
  :config
  (save-place-mode))

;;;; autorevert

(use-package autorevert
  :hook
  (find-file . auto-revert--global-adopt-current-buffer)
  :config
  (setq auto-revert-remote-files t)
  ;; avoid polling when notification is available
  (setq auto-revert-avoid-polling t)
  ;; TODO: configure auto-revert-notify-exclude-dir-regexp
  (global-auto-revert-mode))

;;;; dired

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-lah"))

;;;; dired-x

(use-package dired-x
  :hook
  (dired-mode . dired-omit-mode)
  :init
  (setq dired-x-hands-off-my-keys nil))

;;;; compile

(use-package compile
  :defer t
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save t
        compilation-scroll-output 'first-error)

  (defun +compile--use-pipe ()
    (setq-local process-connection-type nil))

  (add-hook 'compilation-mode-hook #'+compile--use-pipe))

;;;; comint

(use-package comint
  :defer t
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048))

;;;; eshell

(use-package eshell
  :defer t
  :defines (eshell-hist-mode-map)
  :config
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
      (bind-key "M-r" #'consult-history eshell-hist-mode-map))))

;;;; medit

(use-package medit
  :bind
  ("M-s %" . medit-dwim)
  (:map isearch-mode-map
        ("M-s %" . medit-from-isearch))
  :config
  (with-eval-after-load 'embark
    (bind-key "%" #'medit-dwim embark-identifier-map)))

;;;; text-mode

(use-package text-mode
  :defer t
  :config
  (defun +text-mode--capf ()
    (when (fboundp 'cape-dict)
      (add-hook 'completion-at-point-functions #'cape-dict 90 t)))
  (add-hook 'text-mode-hook #'+text-mode--capf))

;;;; nxml-mode

(use-package nxml-mode
  :defer t
  :config
  (defun +nxml-mode--flymake ()
    (when (fboundp 'flymake-xmllint)
      (add-hook 'flymake-diagnostics-functions nil #'flymake-xmllint)))
  (add-hook 'nxml-mode-hook #'+nxml-mode--flymake))

;;;; outline

(use-package outline-minor-faces
  :hook
  (outline-minor-mode . outline-minor-faces-mode))

;;;; adaptive-wrap or visual-wrap

(use-package adaptive-wrap
  :unless (fboundp 'visual-wrap-prefix-mode)
  :hook
  (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package visual-wrap
  :when (locate-library "visual-wrap")
  :hook
  (visual-line-mode . visual-wrap-prefix-mode))

;;;; hl-todo

(use-package hl-todo
  :hook
  ((prog-mode conf-mode) . hl-todo-mode)
  :bind
  (:repeat-map +hl-todo--repeat-map
               ("[" . hl-todo-previous)
               ("]" . hl-todo-next)
               :exit
               ("o" . hl-todo-occur))
  (:map hl-todo-mode-map
        ("C-c t [" . hl-todo-previous)
        ("C-c t ]" . hl-todo-next)
        ("C-c t o" . hl-todo-occur)
        ("C-c t i" . hl-todo-insert)))

;;;; display-line-numbers

(use-package display-line-numbers
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;;;; xref

(use-package xref
  :bind
  ("C-<down-mouse-1>" . nil)
  ("C-M-<down-mouse-1>" . nil)
  ("C-<mouse-1>" . xref-find-definitions-at-mouse)
  ("C-M-<mouse-1>" . xref-find-references-at-mouse)
  :config
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer-at-bottom)
  (setq xref-search-program
        (cond ((executable-find "rg") 'ripgrep)
              ((executable-find "ugrep") 'ugrep)
              (t 'grep))))

;;;; ctags

(use-package ctags-xref
  :bind
  ("C-c t m" . ctags-menu)
  :init
  (add-hook 'xref-backend-functions #'ctags-xref-backend)
  :config
  ;; Override `xref-backend-references' for ctags: if GRTAGS exists,
  ;; find reference using `gtags' backend.
  (cl-defmethod xref-backend-references :around ((_backend (eql 'ctags)) identifier)
    (require 'gtags)
    (if (and (locate-dominating-file default-directory "GRTAGS")
             (executable-find "global" t))
        (xref-backend-references 'gtags identifier)
      (cl-call-next-method))))

;;;; gtags

(use-package gtags
  :bind
  ("C-c t G" . gtags-update)
  :hook
  (after-save . gtags-single-update))

;;;; devdocs

(use-package devdocs
  :bind
  ("C-c d d" . devdocs-lookup)
  ("C-c d i" . devdocs-install)
  ("C-c d p" . devdocs-peruse))

;;;; gtkdoc

(use-package good-doc
  :bind
  ("C-c d g" . good-doc-lookup))

;;;; rust-docs

(use-package rust-docs
  :bind
  ("C-c d r" . rust-docs-lookup))

;;;; javascript

(use-package js
  :defer t
  :config
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
      "C-c C-t" sgml-tag)))

;;;; lisp-mode

(use-package lisp-mode
  :hook
  (lisp-data-mode . electric-pair-local-mode))

;;;; paredit

(use-package paredit
  :hook
  (lisp-data-mode . paredit-mode)
  :bind
  (:map paredit-mode-map
        ("M-s" . nil)))

;;;; elisp-mode

(use-package elisp-mode
  :config
  (setq-default elisp-flymake-byte-compile-load-path load-path)
  :bind
  (:map emacs-lisp-mode-map
        ("C-M-x" . compile-defun)
        ("C-c C-l" . emacs-lisp-native-compile-and-load))
  (:map lisp-interaction-mode-map
        ("C-c C-j" . eval-print-last-sexp)))

;;;; pp

(use-package pp
  :bind
  ("C-x C-e" . pp-eval-last-sexp)
  :config
  (defvar +pp--popon nil)
  (defvar +pp--output-buffer nil)

  (defun +pp--post-command ()
    (and (poponp +pp--popon)
         (not (memq this-command '(pp-eval-last-sexp
                                   pp-eval-expression
                                   pp-macroexpand-last-sexp
                                   pp-macroexpand-expression)))
         (popon-kill +pp--popon)))
  (add-hook 'post-command-hook #'+pp--post-command)

  (defun +pp--copy-output-as-kill ()
    (interactive)
    (kill-new (substring-no-properties (popon-text +pp--popon))))

  (defun +pp--insert-output ()
    (interactive)
    (insert (substring-no-properties (popon-text +pp--popon))))

  (defun +pp--show-buffer ()
    (interactive)
    (display-buffer +pp--output-buffer))

  (defvar-keymap +pp--output-map
    "w" #'+pp--copy-output-as-kill
    "M-w" #'+pp--copy-output-as-kill
    "i" #'+pp--insert-output
    "b" #'+pp--show-buffer
    "q" #'ignore)

  (define-advice pp-display-expression
      (:override (expression out-buffer-name &optional lisp) popon)
    (let* ((lexical lexical-binding)
           (temp-buffer-show-function
            (lambda (buf)
              (with-current-buffer (window-buffer (selected-window))
                (let ((text (with-current-buffer buf (string-trim-right (buffer-string))))
                      (pos (popon-x-y-at-pos (point)))
                      width)
                  (setq width (+ 4 (apply #'max (mapcar #'length (string-lines text)))))
                  (put-text-property 0 1 'display (concat (propertize " => " 'face 'shadow)
                                                          (substring text 0 1))
                                     text)
                  (when (poponp +pp--popon) (popon-kill +pp--popon))
                  (setq +pp--popon (popon-create (cons text width) pos))
                  (message "")          ; clear the message
                  (setq +pp--output-buffer out-buffer-name)
                  (set-transient-map +pp--output-map))))))
      (with-output-to-temp-buffer out-buffer-name
        (if lisp
            (with-current-buffer standard-output
              (pp-emacs-lisp-code expression))
          (pp expression))
        (with-current-buffer standard-output
          (delay-mode-hooks
            (emacs-lisp-mode)
            (setq lexical-binding lexical)
            (setq buffer-read-only nil)
            (setq-local font-lock-verbose nil)
            (font-lock-ensure)))))))

;;;; find-func

(use-package find-func
  :bind
  ("C-x F" . find-function)
  ("C-x V" . find-variable)
  ("C-x K" . find-function-on-key)
  ("C-x L" . find-library))

;;;; cc-mode

(use-package cc-mode
  :defer t
  :config
  (setq c-tab-always-indent nil
        c-insert-tab-function #'completion-at-point)
  (setq-default c-auto-newline t
                c-hungry-delete-key t)
  (defun +cc-mode--hook ()
    (electric-pair-local-mode)
    (add-hook 'flymake-diagnostics-functions #'flymake-clang-tidy nil t))
  (add-hook 'c-mode-common-hook '+cc-mode--hook))


;;;; rust-mode

(use-package rust-mode
  :defer t)

;;;; ruby

(use-package ruby-mode
  :defer t)

;;;; sh-script

(use-package sh-script
  :hook
  (sh-mode . sh-electric-here-document-mode)
  (sh-mode . electric-pair-local-mode)
  (sh-mode . executable-make-buffer-file-executable-if-script-p))

;;;; project

(use-package project
  :defer t
  :config
  (dolist (file '(".project-root" "configure.ac" ".dir-locals.el"
                  "Cargo.toml" "package.json"))
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

  (add-to-list 'project-switch-commands '(project-compile "Compile") t))

;;;; buffer-env
(use-package buffer-env
  :hook
  (hack-local-variables . buffer-env-update)
  (comint-mode . buffer-env-update)
  :config
  (add-to-list 'buffer-env-command-alist '("/\\.nvmrc\\'" . "~/.nvm/nvm-exec env -0"))
  (setq buffer-env-script-name '(".envrc" ".nvmrc" ".env"))

  (defun +buffer-env/clear-cache ()
    "Clear buffer-env cache."
    (interactive)
    (clrhash buffer-env--cache)))

;;;; vc-svn

(use-package vc-svn
  :defer t
  :config

  ;; Use pinentry to enter SVN credentials.  A GUI-based pinentry is needed.
  (setq vc-svn-global-switches
        '("--force-interactive"
          "--config-option"
          "config:auth:password-stores=gpg-agent")))

;;;; magit

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch)
  ("C-x p m" . magit-project-status)
  :defines (magit-credential-cache-daemon-process)
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))
  :config
  (defun +magit--ccdp-no-query ()
    "Avoid query process status on exit."
    (when magit-credential-cache-daemon-process
      (set-process-query-on-exit-flag
       magit-credential-cache-daemon-process nil)))
  (advice-add #'magit-maybe-start-credential-cache-daemon :after '+magit--ccdp-no-query)

  (magit-wip-mode))

;;;; diff-hl

(use-package diff-hl
  :after vc
  :hook
  (tty-setup . diff-hl-margin-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;;;; eat
(use-package eat
  :unless (memq system-type '(windows-nt ms-dos))
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  :bind
  ("C-x p s" . eat-project)
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(eat-project "Eat") t))
  :config
  (setq eat-kill-buffer-on-exit t)

  (with-eval-after-load 'project
    (when-let ((cell (assq 'project-shell project-switch-commands)))
      (setcar cell #'eat-project))
    (bind-key "s" #'eat-project-other-window project-other-window-map))

  (defun eat-send-pass ()
    (interactive)
    (unless eat-terminal
      (user-error "Process not running"))
    (require 'password-store)
    (eat-term-send-string
     eat-terminal
     (password-store-get (password-store--completing-read t)))
    (eat-self-input 1 'return)))

;;;; with-editor

(use-package with-editor
  :bind
  ("<remap> <async-shell-command>" . with-editor-async-shell-command)
  ("<remap> <shell-command>" . with-editor-shell-command)
  :hook
  ((eshell-mode shell-mode term-exec vterm-mode) . with-editor-export-editor)
  :init
  (defun +with-editor--export-editor-to-eat (proc)
    (require 'with-editor)
    (if with-editor-emacsclient-executable
        (with-editor
          (with-editor--setup)
          (while (accept-process-output proc 1 nil t))
          (when-let ((v (getenv "EDITOR")))
            (eat-term-send-string eat-terminal (format " export EDITOR=%S" v))
            (eat-self-input 1 'return))
          (when-let ((v (getenv "EMACS_SERVER_FILE")))
            (eat-term-send-string eat-terminal (format " export EMACS_SERVER_FILE=%S" v))
            (eat-self-input 1 'return))
          (eat-term-send-string eat-terminal " clear")
          (eat-self-input 1 'return))
      (error "Cannot use sleeping editor in this buffer")))
  (add-hook 'eat-exec-hook '+with-editor--export-editor-to-eat))

;;;; pyim

(use-package pyim
  :defer t
  :functions
  pyim-cregexp-build
  :init
  (defun +orderless-pinyin (component)
    (require 'pyim)
    (pyim-cregexp-build component 3 t))

  (with-eval-after-load 'orderless
    (add-to-list 'orderless-affix-dispatch-alist
                 '(?` . +orderless-pinyin)))

  ;; Set the default input method of Chinese language environments to `pyim'.
  (set-language-info "Chinese-GB18030" 'input-method "pyim")
  (set-language-info "Chinese-GBK" 'input-method "pyim")
  (set-language-info "Chinese-GB" 'input-method "pyim")

  ;; UTF-8 is preferred on Unix-like systems.
  (set-language-info "UTF-8" 'input-method "pyim")

  :config
  (defvar +pyim--corfu nil)
  (defun +pyim--activate ()
    (when (boundp 'corfu-auto)
      (setq-local +pyim--corfu corfu-auto)
      (setq-local corfu-auto nil)))
  (defun +pyim--deactivate ()
    (when (boundp 'corfu-auto)
      (setq-local corfu-auto +pyim--corfu)))
  (add-hook 'pyim-activate-hook '+pyim--activate)
  (add-hook 'pyim-deactivate-hook '+pyim--deactivate))

(use-package pyim-basedict
  :after pyim
  :config
  (pyim-basedict-enable))

;;;; kinsoku

(use-package kinsoku
  :defer t
  :config
  (setq word-wrap-by-category t))

;;;; term-keys

(use-package term-keys
  :defer t
  :defines
  term-keys/prefix
  :init
  (setq term-keys/prefix "\033\035")      ; ^[^]

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
               (not term-keys-mode))
      (with-selected-frame (car (frames-on-display-list terminal))
        (define-key input-decode-map term-keys/prefix '+term-keys--autoload))))

  (add-hook 'tty-setup-hook '+term-keys--tty-setup)
  (mapc '+term-keys--tty-setup (terminal-list))

  :config
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
              (cons [?\e ?\C-\]] eat-semi-char-non-bound-keys)))))

;;;; xterm

(use-package term/xterm
  :defer t
  :config
  (setq xterm-set-window-title t))

;;;; xt-mouse

(use-package xt-mouse
  :hook
  (tty-setup . xterm-mouse-mode)
  :init
  (when (eq (framep-on-display) t)
    (require 'xt-mouse))
  :config
  (xterm-mouse-mode))

;;;; clipetty

(use-package clipetty
  :hook
  (tty-setup . clipetty-mode)
  :init
  (when (eq (framep-on-display) t)
    (require 'clipetty))
  :config
  (global-clipetty-mode))

;;;; which-key

(use-package which-key
  :disabled
  :config
  (which-key-mode))

;;;; repeat

(use-package repeat
  :defer t
  :init
  (defun +repeat--post-command ()
    (when (function-get this-command 'repeat-map)
      (message "Command %S has a `repeat-map'" this-command)
      (require 'repeat)
      (repeat-post-hook)))
  (add-hook 'post-command-hook '+repeat--post-command)
  :config
  (remove-hook 'post-command-hook '+repeat--post-command)
  (repeat-mode))

;;;; savehist

(use-package savehist
  :hook
  (minibuffer-setup . savehist-minibuffer-hook)
  (kill-emacs . savehist-autosave)
  :config
  (savehist-mode))

;;;; auth-sources

(use-package auth-source
  :defer t
  :init
  (setq auth-sources '("~/.authinfo.gpg"))
  :config
  (auth-source-forget-all-cached))

(use-package auth-source-pass
  :after auth-source
  :config
  (auth-source-pass-enable))

;;;; lin

(use-package lin
  :defer t
  :init
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
           tabulated-list-mode-hook tar-mode-hook))
  (dolist (hook lin-mode-hooks)
    (add-hook hook #'lin-mode))
  :config
  (setq lin-face 'lin-magenta)
  (lin-global-mode))

;;;; emacs-server

(use-package server
  :defer 30
  :config
  ;; Workaround windows encoding issue
  (when (memq system-type '(windows-nt ms-dos))
    (defun +server--process-filter-coding-system (&rest args)
      (let ((file-name-coding-system locale-coding-system))
        (apply args)))
    (add-hook #'server-process-filter :around '+server--process-filter-coding-system))

  (unless (server-running-p)
    (server-start)))

;;;; org-protocol

(use-package org-protocol
  :after server)

;;;; epg

(use-package epg
  :defer t
  :config
  (setq epg-pinentry-mode 'loopback))

(use-package epa
  :defer t
  :config
  (setq epa-keys-select-method 'minibuffer))

;;;; tui

(use-package tui
  :unless (memq system-type '(ms-dos windows-nt))
  :bind
  ("C-c t t" .  tui-run)
  ("C-c t r" .  tui-rg)
  ("C-c t g" .  tui-ugrep)
  ("C-c t y" .  tui-yazi)
  ("C-c t k" .  tui-kill)
  ("C-c t l" .  tui-line)
  ("C-c t f" .  tui-find)
  ("C-c t d" .  tui-locate)
  :config
  (setf (alist-get "^\\*tui-" display-buffer-alist nil nil #'equal)
        '((display-buffer-same-window))))

;;;; gptel

(use-package gptel
  :bind
  ("C-c g" . gptel-send))

(use-package gptel-quick
  :defer t
  :init
  (with-eval-after-load 'embark
    (bind-key "?" #'gptel-quick embark-general-map)))

;;;; logos

(use-package logos
  :bind
  ("<f8>" . logos-focus-mode)
  ("<remap> <narrow-to-region>" . logos-narrow-dwim)
  ("<remap> <forward-page>" . logos-forward-page-dwim)
  ("<remap> <backward-page>" . logos-backward-page-dwim)
  (:map logos-focus-mode-map
        ("<left>" . logos-backward-page-dwim)
        ("<right>" . logos-forward-page-dwim))
  :config
  (setq logos-outlines-are-pages t)

  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti nil)

  (defun logos-focus--narrow ()
    (when logos-focus-mode
      (logos--narrow-to-page 0)
      (make-local-variable 'logos--restore)
      (push #'widen logos--restore)))

  (add-hook 'logos-focus-mode-hook #'logos-focus--narrow)

  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter)))))

;;;; gpg-agent

(use-package gpg-agent
  :unless (memq system-type '(ms-dos windows-nt))
  :after (:any vc magit comint)
  :config
  (gpg-agent-terminal-start))

;;;; eww

(use-package eww
  :defer t
  :config

  (defun eww+miniflux-trim ()
    (when (string-match-p "^https://miniflux\\." (eww-current-url))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when-let ((match (text-property-search-forward 'shr-target-id "page-header-title" 'member)))
            (delete-region (point-min) (prop-match-beginning match)))))))

  (defun eww+kagi-trim ()
    (when (string-match-p "^https://kagi\\.com" (eww-current-url))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when-let ((match (text-property-search-forward 'shr-target-id "tonav" #'member)))
            (delete-region (prop-match-beginning match) (prop-match-end match)))
          ))))

  (defun eww-reset-current-bookmark ()
    (when (and bookmark-current-bookmark
               (not (equal (eww-current-url) (bookmark-location bookmark-current-bookmark))))
      (setq bookmark-current-bookmark nil)))

  (add-hook 'eww-after-render-hook 'eww+miniflux-trim)
  (add-hook 'eww-after-render-hook 'eww+kagi-trim)

  (with-eval-after-load 'bookmark
    (add-hook 'eww-after-render-hook 'eww-reset-current-bookmark)))

;;;; bookmark

(defvar pp-default-function)

(use-package bookmark
  :defer t
  :config
  (setq bookmark-save-flag 1
        bookmark-watch-bookmark-file 'silent
        bookmark-version-control t)
  (defun +bookmark--pp-28 (&rest args)
    (let ((pp-default-function 'pp-28))
      (apply args)))
  (advice-add #'bookmark-write-file :around '+bookmark--pp-28))

(use-package bookmark-extras
  :bind
  ("C-x r u" . url-bookmark-add))

;;;; org

(use-package org
  :bind
  ("C-c l" . org-store-link))

(use-package org-agenda
  :bind
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c"))))

(use-package org-capture
  :bind
  ("C-c c" . org-capture)
  :config
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

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;;;; denote

(use-package denote
  :bind
  ("C-c n n" . denote)
  ("C-c n c" . denote-region)           ; "contents" mnemonic
  ("C-c n N" . denote-type)
  ("C-c n d" . denote-date)
  ("C-c n z" . denote-signature)
  ("C-c n s" . denote-subdirectory)
  ("C-c n t" . denote-template)
  ("C-c n o" . denote-open-or-create)
  ("C-c n O" . denote-open-or-create-with-command)

  ("C-c n i" . denote-link)
  ("C-c n I" . denote-add-links)
  ("C-c n b" . denote-backlinks)
  ("C-c n f f" . denote-find-link)
  ("C-c n f b" . denote-find-backlink)

  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter)

  :config
  (with-eval-after-load 'dired
    (bind-keys :map dired-mode-map
               ("C-c C-d C-i" . denote-link-dired-marked-notes)
               ("C-c C-d C-r" . denote-dired-rename-files)
               ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
               ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))))

;;;; copilot

(use-package copilot
  :bind
  (:map copilot-mode-map
        ("<tab>" . copilot-accept-completion)
        ("C-<tab>" . copilot-accept-completion-by-word)))

;;;; browser-hist

(use-package browser-hist
  :bind
  ("M-s b" . browser-hist-search)
  :config
  (setf (alist-get 'zen browser-hist-db-paths nil t)
        (cond ((memq system-type '(cygwin windows-nt ms-dos))
               "$APPDATA/zen/Profiles/*/places.sqlite")))
  (setf (alist-get 'zen browser-hist--db-fields)
        '("title" "url" "moz_places" "ORDER BY last_visit_date desc"))
  (setq browser-hist-default-browser 'zen))

;;;; vundo

(use-package vundo
  :defer
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


;;;; post-init

(defvar post-init-file (locate-user-emacs-file "post-init.el"))

(when (file-exists-p post-init-file)
  (load post-init-file nil t))

;; Local Variables:
;; eval: (outline-minor-mode)
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
