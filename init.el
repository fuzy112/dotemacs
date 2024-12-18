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
  (load early-init-file))

;;;; pre-init

(defvar pre-init-file (locate-user-emacs-file "pre-init.el")
  "The file to load before the init file.")

(defvar straight-current-profile)
(when (file-exists-p pre-init-file)
  (let ((straight-current-profile 'user))
    (load pre-init-file nil t)))

(setq straight-current-profile 'dotemacs)


;;;; keymap

(eval-when-compile (require 'bind-key))

(bind-keys :prefix "C-c t"
           :prefix-map tool-map)
(bind-keys :prefix "C-c d"
           :prefix-map doc-map)
(bind-keys :prefix "C-c f"
           :prefix-map file-map)
(bind-keys :prefix "C-c T"
           :prefix-map toggle-map)

(defalias 'search-map search-map)
(bind-key "M-s" 'search-map)

(defalias 'goto-map goto-map)
(bind-key "M-g" 'goto-map)


;;;; meow-edit

(require 'meow)

(defun meow-setup ()
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
   '("c" . meow-change) '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line) '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab) '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above) '("j" . meow-join)
   '("k" . meow-kill) '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol) '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block) '("O" . meow-to-block)
   '("p" . meow-prev) '("P" . meow-prev-expand)
   '("q" . meow-quit) '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab) '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit) '("w" . meow-next-word)
   '("W" . meow-next-symbol) '("x" . meow-save)
   '("X" . meow-sync-grab) '("y" . meow-yank)
   '("z" . meow-pop-selection) '("'" . repeat)
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

(bind-keys :prefix "C-c S"
           :prefix-map straight-prefix-map
           :prefix-docstring "Prefix map for straight.el operations."
           ("c" . straight-check-package)
           ("C" . straight-check-all)
           ("p" . straight-pull-package)
           ("P" . straight-pull-all)
           ("f" . straight-fetch-package)
           ("F" . straight-fetch-all)
           ("b" . straight-rebuild-package)
           ("B" . straight-rebuild-all)
           ("v" . straight-freeze-versions)
           ("V" . straight-thaw-versions)
           ("u" . straight-use-package)
           ("d" . straight-visit-package)
           ("w" . straight-visit-package-website)
           ("g" . +straight/visit-package-repository))

(defvar straight--recipe-cache)
(defun +straight/visit-package-repository (pkg)
  (interactive (list (straight--select-package "Visit: ")))
  (let ((repo (plist-get (gethash pkg straight--recipe-cache)
                         :local-repo)))
    (magit-status-setup-buffer (straight--repos-dir repo))))

(define-prefix-command 'straight-x-prefix-map nil "Straight-X")
(autoload 'straight-x-fetch-all "straight-x" nil t)

(bind-keys :prefix "C-c S x"
           :prefix-map straight-x-prefix-map
           ("f" . straight-x-fetch-all))

;;;; faces
(unless (eq (framep-on-display) t)
  (with-demoted-errors "Failed to setup fonts for Chinese characters: %S"
    (set-fontset-font (frame-parameter nil 'font) 'han "Sarasa Gothic CL")
    (set-fontset-font (frame-parameter nil 'font) 'cjk-misc "Sarasa Gothic CL")))
(set-fontset-font t 'han "Sarasa Gothic CL")
(set-face-attribute 'default nil :family "Iosevka SS04")
(set-face-attribute 'fixed-pitch nil :family "Iosevka SS04")
(set-face-attribute 'variable-pitch nil :family "Sarasa UI CL")
(add-to-list 'face-font-family-alternatives '("Sarasa Gothic CL" "Iosevka SS04"))
(add-to-list 'face-font-family-alternatives '("Sarasa UI CL" "Sarasa Gothic CL" "Iosevka SS04"))
(setopt face-font-family-alternatives (append face-font-family-alternatives nil))

;;;; nerd-icons

(add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-mode)
(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
(add-hook 'dired-mode-hook #'nerd-icons-multimodal-mode)
(add-hook 'archive-mode-hook #'nerd-icons-multimodal-mode)
(add-hook 'tar-mode-hook #'nerd-icons-multimodal-mode)

(ignore-errors
  (nerd-icons-set-font))

;;;; pixel-scroll

(defun +pixel-scroll--autoload ()
  (interactive)
  (require 'pixel-scroll)
  (let ((events (mapcar (lambda (ev) (cons t ev))
                        (listify-key-sequence (this-command-keys)))))
    (setq unread-command-events (append events unread-command-events))))
(bind-key "<wheel-down>" #'+pixel-scroll--autoload)
(with-eval-after-load 'pixel-scroll
  (unbind-key "<wheel-down>")
  (pixel-scroll-precision-mode))

;;;; window
(fset 'window-prefix-map window-prefix-map)
(bind-key "C-c w" #'window-prefix-map)
(bind-key "q" #'quit-windows-on window-prefix-map)
(setq kill-buffer-quit-windows t
      quit-restore-window-no-switch t)

;;;; quick-window

(autoload 'quick-window-jump "quick-window" nil t)
(bind-key "M-o" #'quick-window-jump)

;;;; help
(setq help-enable-variable-value-editing t
      help-enable-completion-autoload nil)

(when (fboundp 'shortdoc-help-fns-examples-function)
  (add-hook 'help-fns-describe-function-functions
            #'shortdoc-help-fns-examples-function 50))

;;;; emacs-lock-mode

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;;;; backup

(autoload 'list-backups "backup" nil t)
(autoload 'backup-list-backups "backup" nil t)
(bind-key "C-c B" #'list-backups)

;;;; breadcrumb

(defun +breadcrumb--prog-mode ()
  (setq-local header-line-format '((:eval (breadcrumb-project-crumbs))
                                   ": "
                                   (:eval (breadcrumb-imenu-crumbs)))))
(defun +breadcrumb--text-mode ()
  (setq-local header-line-format '((:eval (breadcrumb-imenu-crumbs)))))

(add-hook 'text-mode-hook #'+breadcrumb--text-mode)
(add-hook 'conf-mode-hook #'+breadcrumb--text-mode)
(add-hook 'prog-mode-hook #'+breadcrumb--prog-mode)

;;;; orderless

(eval-when-compile (require 'orderless))
(orderless-define-completion-style orderless+flex
  (orderless-matching-styles '(orderless-flex)))
(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))
(setq completion-styles '(orderless basic))
(setq completion-category-overrides
      '((file . ((styles . (basic partial-completion))))
        (symbol . ((styles . (orderless+flex))))
        (symbol-help . ((styles . (orderless+flex))))
        (command . ((styles . (orderless+initialism))))
        (variable . ((styles . (orderless+initialism))))
        (eglot . ((styles . (orderless))))
        (eglot-capf . ((styles . (orderless))))
        (magit-rev . ((styles . (orderless+flex))))))

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

(with-eval-after-load 'orderless
  (add-to-list 'orderless-style-dispatchers #'+orderless--consult-dispatch))

;;;; vertico

(setq enable-recursive-minibuffers t)

(autoload 'vertico--advice "vertico.el")
(advice-add #'completing-read-default :around #'vertico--advice)
(advice-add #'completing-read-multiple :around #'vertico--advice)

(with-eval-after-load 'vertico
  (require 'orderless)
  (setq vertico-quick1 "htnsd"
        vertico-quick2 "ueoai")
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)
  (vertico-mode)
  (bind-key "M-R" #'vertico-repeat)
  (bind-keys :map vertico-map
             ;; vertico-repeat
             ("M-P" . vertico-repeat-previous)
             ("M-N" . vertico-repeat-next)
             ;; vertico-directory
             ("RET" . vertico-directory-enter)
             ("DEL" . vertico-directory-delete-char)
             ("M-DEL" . vertico-directory-delete-word)
             ;; vertico-quick
             ("C-q" . vertico-quick-insert)
             ("M-q" . vertico-quick-exit)))

;;;; marginalia

(autoload 'marginalia--minibuffer-setup "marginalia.el")
(add-hook 'minibuffer-setup-hook #'marginalia--minibuffer-setup)
(bind-key "M-A" #'marginalia-cycle minibuffer-local-map)

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-prompt-categories
               '("\\<info manuals\\>" . info-manual))
  (add-to-list 'marginalia-prompt-categories
               '("\\<manual name\\>" . info-manual))
  (add-to-list 'marginalia-prompt-categories
               '("\\<Log rev,s\\>" . magit-rev))
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
(with-eval-after-load 'corfu
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
  (bind-keys :map corfu-map
             ("C-q" . corfu-quick-insert)
             ("M-q" . corfu-quick-complete)
             ("TAB" . corfu-next)
             ("S-TAB" . corfu-previous)
             ("<backtab>" . corfu-previous)))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'corfu-history))

(unless (featurep 'tty-child-frames)
  (add-hook 'tty-setup-hook #'corfu-terminal-mode)
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

;;;; cape

(bind-key "C-c e" #'cape-prefix-map)
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)

;;;; dabbrev

(defvar dabbrev-ignored-buffer-regexps)
(defvar dabbrev-ignored-buffer-modes)
(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;;;; tempel

(bind-keys ("M-+" . tempel-complete)
           ("M-*" . tempel-insert))
(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))
(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)
(defvar tempel-path)
(defvar tempel-user-elements)
(with-eval-after-load 'tempel
  (setq tempel-path (concat user-emacs-directory "/templates/*.eld"))
  (defun tempel-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  (add-to-list 'tempel-user-elements #'tempel-include))

;;;; embark

(bind-keys ("C-." . embark-act)
           ("C-c a" . embark-act))
(setq embark-cycle-key ".")
(setq prefix-help-command #'embark-prefix-help-command)

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

(defvar ansi-color-apply-face-function)
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

(defvar embark-indicators)
(defvar embark-file-map)
(defvar embark-bookmark-map)
(defvar embark-region-map)

(with-eval-after-load 'embark
  (bind-key "#" '+embark/find-file-as-root embark-file-map)
  (bind-key "W" '+embark/eww-open-bookmark embark-bookmark-map)
  (bind-key "u" '+embark/browse-url-open-bookmark embark-bookmark-map)
  (bind-key "[" '+embark/apply-ansi-color embark-region-map))

;;;; consult

(bind-keys ("C-c M-x" . consult-mode-command)
           ("C-c h" . consult-history)
           ("C-c k" . consult-kmacro)
           ("C-c m" . consult-man)
           ("C-c i" . consult-info)
           ("<remap> <Info-search>" . consult-info)
           ;; C-x bindings in `ctl-x-map'
           ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
           ("C-c b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
           ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
           ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop) ;; orig. yank-pop
           ;; M-g bindings in `goto-map'
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)   ;; orig. goto-line
           ("M-g M-g" . consult-goto-line) ;; orig. goto-line
           ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings in `search-map'
           ("M-s d" . consult-find) ;; Alternative: consult-fd
           ("M-s c" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history))

(bind-keys :map isearch-mode-map
           ("M-e" . consult-isearch-history)
           ("M-s e" . consult-isearch-history)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi))

(bind-keys :map minibuffer-local-map
           ("M-s" . consult-history)
           ("M-r" . consult-history))

(add-hook 'completion-list-mode-hook #'consult-preview-at-point)

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

(setq consult-preview-key "M-.")
(setq consult-narrow-key "<") ;; "C-+"

(defvar orderless-match-faces)
(defun +consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (cdr (orderless-compile input)))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str)
     (let ((orderless-match-faces orderless-match-faces))
       (setq orderless-match-faces (vconcat '(consult-highlight-match) orderless-match-faces))
       (orderless--highlight input t str)))))
(setq-default consult--regexp-compiler #'+consult--orderless-regexp-compiler)

(with-eval-after-load 'consult
  ;; consult-customize is not autoloaded
  (eval '(consult-customize
          consult-xref consult-ripgrep consult-grep consult-git-grep
          consult-line consult-focus-lines consult-keep-lines
          consult-imenu
          :preview-key '(:debounce 0.2 any))
        lexical-binding)
  (cl-pushnew #'url-bookmark-jump (cddr (assoc ?w consult-bookmark-narrow))))


(declare-function grep--heading-filter "grep.el")
(defun +embark-consult-export-grep--headings (&rest _)
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (grep--heading-filter))))

(declare-function embark-consult-export-grep "embark-consult.el" (arg1))
(with-eval-after-load 'embark-consult
  (require 'grep)
  (when (fboundp 'grep--heading-filter)
    (advice-add #'embark-consult-export-grep :after #'+embark-consult-export-grep--headings)))


(bind-key "C-x C-d" #'consult-dir)
(bind-keys :map minibuffer-local-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))

;;;; windmove

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
(with-eval-after-load 'windmove
  (windmove-default-keybindings))

;;;; popper

(eval-when-compile (require 'popper))
(autoload 'popper-toggle-type "popper.el" nil t)
(bind-keys ("C-`" . popper-toggle)
           ("M-`" . popper-cycle)
           ("C-M-`" . popper-toggle-type))
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

(with-eval-after-load 'popper
  (setq display-buffer-alist (assq-delete-all '+popper--popup-p display-buffer-alist))

  ;; delay select-window to post-command-hook
  (defvar +popper--delayed-window nil)
  (defun +popper--select-delayed-window ()
    (when +popper--delayed-window
      (select-window +popper--delayed-window)
      (setq +popper--delayed-window nil)))
  (add-hook 'post-command-hook '+popper--select-delayed-window 90)
  (defun +popper--select-popup-delayed (buf alist)
    (if this-command
        (setq +popper--delayed-window (popper-display-popup-at-bottom buf alist))
      (select-window (popper-display-popup-at-bottom buf alist))))
  (setq popper-display-function '+popper--select-popup-delayed)
  (with-eval-after-load 'project
    (setq popper-group-function #'popper-group-by-project))
  (popper-mode)
  (popper-echo-mode))

;;;; ibuffer

(bind-key "<remap> <list-buffers>" #'ibuffer-jump)

;;;; apheleia

(bind-keys ("C-x x /" . apheleia-format-buffer)
           ("C-c C-/" . apheleia-format-buffer))

;;;; ws-butler

(add-hook 'find-file-hook #'ws-butler-mode)
(with-eval-after-load 'ws-butler
  (remove-hook 'find-file-hook #'ws-butler-mode)
  (ws-butler-global-mode))

;;;; whitespace

(eval-when-compile (require 'whitespace))
(dolist (hook '(prog-mode-hook conf-mode-hook yaml-mode-hook))
  (add-hook hook #'whitespace-mode))
(setq whitespace-style '(face trailing empty indentation space-before-tab space-after-tab))

;;;; recentf

(eval-when-compile (require 'recentf))
(autoload 'recentf-track-opened-file "recentf.el"
  "Insert the name of the file just opened or written into the recent list." )
(add-hook 'find-file-hook #'recentf-track-opened-file)
(setq recentf-max-saved-items 128)
(with-eval-after-load 'recentf
  (let ((inhibit-message t))
    (recentf-mode)))
(with-eval-after-load 'consult
  (setf (plist-get consult--source-recent-file :enabled)
        (lambda () (require 'recentf) (symbol-value 'recentf-mode))))

(bind-key "C-c f R" #'recentf-open)

;;;; saveplace

(autoload 'save-place-find-file-hook "saveplace.el")
(autoload 'save-place-dired-hook "saveplace.el"
  "Position point in a Dired buffer according to its saved place.
This is run via ‘dired-initial-position-hook’, which see." )
(add-hook 'find-file-hook #'save-place-find-file-hook)
(add-hook 'dired-initial-position-hook #'save-place-dired-hook)
(with-eval-after-load 'saveplace
  (save-place-mode))

;;;; autorevert

(eval-when-compile (require 'autorevert))
(autoload 'auto-revert--global-adopt-current-buffer "autorevert.el"
  "Consider tracking current buffer in a running Global Auto-Revert mode." )
(add-hook 'find-file-hook #'auto-revert--global-adopt-current-buffer)

(setq auto-revert-remote-files t
      auto-revert-avoid-polling t)
(with-eval-after-load 'autorevert
  (global-auto-revert-mode))

;;;; dired

(eval-when-compile (require 'dired)
                   (require 'dired-x))
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-listing-switches "-lah"
      dired-hide-details-hide-absolute-location t
      dired-x-hands-off-my-keys nil)

;;;; compile

(eval-when-compile (require 'compile))
(setq compilation-always-kill t
      compilation-ask-about-save t
      compilation-scroll-output 'first-error)

;;;; comint

(eval-when-compile (require 'comint))
(setq comint-prompt-read-only t
      comint-buffer-maximum-size 2048)

;;;; eshell

(defun +eshell/here ()
  (interactive)
  (defvar eshell-buffer-name)
  (let ((eshell-buffer-name (format "*%s : eshell*" (abbreviate-file-name default-directory)))
        (display-comint-buffer-action '(() (inhibit-same-window . t))))
    (eshell)))
(bind-key "C-c t e" #'+eshell/here)

(defvar eshell-hist-mode-map)
(with-eval-after-load 'eshell
  (setopt eshell-scroll-to-bottom-on-input t
          eshell-history-size 8192
          eshell-save-history-on-exit t)
  (setopt eshell-modules-list (seq-union '(eshell-tramp) (symbol-value 'eshell-modules-list)))
  (defun +eshell--capf ()
    (when (fboundp 'cape-history)
      (add-hook 'completion-at-point-functions #'cape-history 50 t)))
  (add-hook 'eshell-mode-hook '+eshell--capf)
  (with-eval-after-load 'esh-hist
    (when (fboundp 'consult-history)
      (bind-key "M-r" #'consult-history eshell-hist-mode-map))))

;;;; text-mode

(defun +text-mode--capf ()
  (when (fboundp 'cape-dict)
    (add-hook 'completion-at-point-functions #'cape-dict 90 t)))
(add-hook 'text-mode-hook #'+text-mode--capf)

;;;; nxml-mode

(with-eval-after-load 'nxml-mode
  (defun +nxml-mode--flymake ()
    (when (fboundp 'flymake-xmllint)
      (add-hook 'flymake-diagnostics-functions nil #'flymake-xmllint)))
  (add-hook 'nxml-mode-hook #'+nxml-mode--flymake))

;;;; outline

(defun +outline-minor-faces ()
  (unless (derived-mode-p 'help-mode)
    (outline-minor-faces-mode)))
(add-hook 'outline-minor-mode-hook '+outline-minor-faces)

(bind-key "C-c T o" #'outline-minor-mode)

;;;; adaptive-wrap or visual-wrap

(static-if (locate-library "visual-wrap")
    (add-hook 'visual-line-mode-hook #'visual-wrap-prefix-mode)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(bind-key "C-c T v" #'visual-line-mode)

;;;; hl-todo

(bind-keys ("C-c T t" . hl-todo-mode)
           ("C-c t [" . hl-todo-previous)
           ("C-c t ]" . hl-todo-next)
           ("C-c t o" . hl-todo-occur)
           ("C-c t i" . hl-todo-insert))

(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'conf-mode-hook #'hl-todo-mode)

(defvar-keymap +hl-todo-repeat-map
  "[" #'hl-todo-previous
  "]" #'hl-todo-next
  "o" #'hl-todo-occur)
(put #'hl-todo-previous 'repeat-map +hl-todo-repeat-map)
(put #'hl-todo-previous 'repeat-map +hl-todo-repeat-map)

;;;; display-line-numbers

(bind-key "C-c T l" #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;;;; display-fill-column-indicator-mode

(bind-key "C-c T f" #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;; eglot

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

(with-eval-after-load 'eglot
  (eglot-tempel-mode))

;;;; xref

(bind-keys ("C-<down-mouse-1>" . nil)
           ("C-M-<down-mouse-1>" . nil)
           ("C-<mouse-1>" . xref-find-definitions-at-mouse)
           ("C-M-<mouse-1>" . xref-find-references-at-mouse))

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
      xref-auto-jump-to-first-definition t)

;;;; ctags

(autoload 'ctags-menu "ctags-menu" nil t)
(bind-key "C-c t m" #'ctags-menu)

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

(with-eval-after-load 'ctags-xref
  (with-eval-after-load 'cc-mode
    (require 'ctags-xref-c)))

;;;; gtags

(autoload 'gtags-update "gtags.el" nil t)
(autoload 'gtags-single-update "gtags.el" nil t)
(add-hook 'after-save-hook #'gtags-single-update)

;;;; devdocs

(bind-keys ("C-c d d" . devdocs-lookup)
           ("C-c d i" . devdocs-install)
           ("C-c d p" . devdocs-peruse))

;;;; gtkdoc

(autoload 'good-doc-lookup "good-doc" nil t)
(bind-key "C-c d g" #'good-doc-lookup)

;;;; rust-docs

(autoload 'rust-docs-lookup "rust-docs" nil t)
(bind-key "C-c d r" #'rust-docs-lookup)

;;;; javascript

(define-advice js-jsx-enable (:after () comments)
  (setq-local comment-region-function #'js-jsx--comment-region))

(define-advice js-jsx-enable (:after () sgml)
  (eval-when-compile (require 'sgml-mode))
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map)
    (define-key map (kbd "C-c C-b") #'sgml-skip-tag-backward)
    (define-key map (kbd "C-c C-d") #'sgml-delete-tag)
    (define-key map (kbd "C-c C-e") #'sgml-close-tag)
    (define-key map (kbd "C-c C-f") #'sgml-skip-tag-forward)
    (define-key map (kbd "C-c C-o") #'sgml-tag)
    (define-key map (kbd "C-c C-t") #'sgml-tag)))

;;;; sly

;;;; geiser

;;;; paren-face

(add-hook 'lisp-data-mode-hook #'paren-face-mode)
(add-hook 'scheme-mode-hook #'paren-face-mode)

;;;; puni

(add-hook 'prog-mode-hook #'puni-mode)
(add-hook 'conf-mode-hook #'puni-mode)
(add-hook 'puni-mode-hook #'electric-pair-local-mode)
(bind-keys :map puni-mode-map
           ("C-)" . puni-slurp-forward)
           ("C-(" . puni-slurp-backward)
           ("C-}" . puni-barf-forward)
           ("C-{" . puni-barf-backward))

;;;; elisp-mode

(defun +elisp-mode--setup ()
    (setq-local prettify-symbols-alist '(("lambda" . ?λ)))
    (prettify-symbols-mode)
    (setq-local elisp-flymake-byte-compile-load-path (symbol-value 'load-path)))
(add-hook 'emacs-lisp-mode-hook #'+elisp-mode--setup)

(with-eval-after-load 'elisp-mode
  (static-if (native-comp-available-p)
      (bind-key "C-c C-l" #'emacs-lisp-native-compile-and-load emacs-lisp-mode-map))
  (bind-key "C-c C-j" #'eval-print-last-sexp lisp-interaction-mode-map))

;;;; pp

(bind-key "M-:" #'pp-eval-expression)

;;;; pp-posframe

(autoload 'pp-posframe-eval-last-sexp "pp-posframe.el"
  "Evaluate sexp before point; display the value in a posframe." t)
(autoload 'pp-posframe-compile-defun "pp-posframe.el"
  "Compile and evaluate the current top-level form.
Display the result in a posframe." t)
(autoload 'pp-posframe-macroexpand-last-sexp "pp-posframe.el"
  "Macroexpand the sexp before point; display the result in a posframe." t)
(bind-key "C-x C-e" #'pp-posframe-eval-last-sexp)
(with-eval-after-load 'elisp-mode
  (bind-keys :map emacs-lisp-mode-map
             ("C-M-x" . pp-posframe-compile-defun)
             ("C-c M-e" . pp-posframe-macroexpand-last-sexp)))

;;;; find-func

(bind-keys ("C-x F" . find-function)
           ("C-x V" . find-variable)
           ("C-x K" . find-function-on-key)
           ("C-x L" . find-library)
           ("C-x 4 F" . find-function-other-window)
           ("C-x 4 V" . find-variable-other-window)
           ("C-x 4 K" . find-function-on-key-other-window)
           ("C-x 4 L" . find-library-other-window)
           ("C-x 5 F" . find-function-other-frame)
           ("C-x 5 V" . find-variable-other-frame)
           ("C-x 5 K" . find-function-on-key-other-frame)
           ("C-x 5 L" . find-library-other-frame))

;;;; eldoc

(with-eval-after-load 'eldoc
  (eldoc-add-command
   'magit-next-line 'magit-previous-line
   'magit-section-forward 'magit-section-backward
   'magit-section-forward-sibling 'magit-section-backward-sibling
   'magit-blame-next-chunk 'magit-blame-previous-chunk))

;;;; cc-mode

(setq c-tab-always-indent nil
      c-insert-tab-function #'completion-at-point)
(defun +cc-mode--hook ()
  (add-hook 'flymake-diagnostics-functions #'flymake-clang-tidy nil t))
(add-hook 'c-mode-common-hook '+cc-mode--hook)

;;;; rust-mode

(define-advice rust--compile (:around (&rest args) project-prefix-buffer-name)
  (let ((compilation-buffer-name-function #'project-prefixed-buffer-name))
    (apply args)))

;;;; ruby

;;;; sh-script

(add-hook 'sh-mode-hook #'sh-electric-here-document-mode)
(add-hook 'save-file-hook #'executable-make-buffer-file-executable-if-script-p)

;;;; project

(fset 'project-prefix-map project-prefix-map)
(bind-key "C-c p" #'project-prefix-map)
(static-if (commandp 'project-prefix-or-any-command)
    (setq project-switch-commands 'project-prefix-or-any-command))

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

(with-eval-after-load 'project
  (dolist (file '(".project-root" "configure.ac" "Cargo.toml" "package.json"))
    (add-to-list 'project-vc-extra-root-markers file))
  (when (and (not (functionp project-switch-commands))
             (consp project-switch-commands))
    (add-to-list 'project-switch-commands '(project-compile "Compile") t)))

;;;; buffer-env

(dolist (hook '(hook-local-variables-hook comint-mode-hook))
  (add-hook hook #'buffer-env-update))
(defvar buffer-env--cache)
(defun +buffer-env/clear-cache ()
  "Clear buffer-env cache."
  (interactive)
  (clrhash buffer-env--cache))
(defvar buffer-env-command-alist)
(with-eval-after-load 'buffer-env
  (setf (alist-get "/\\.nvmrc\\'" buffer-env-command-alist nil nil #'equal) "~/.nvm/nvm-exec env -0")
  (setopt buffer-env-script-name '(".envrc" ".nvmrc" ".env")))

;;;; vc

(setq vc-follow-symlinks t)
(bind-key "C-c v" #'vc-prefix-map)
(with-eval-after-load 'vc
  (require 'diff-hl))

(setq vc-svn-global-switches
      '("--force-interactive"
        "--config-option"
        "config:auth:password-stores=gpg-agent")
      vc-svn-diff-switches '("-x" "-u -p"))

;;;; magit

(bind-keys ("C-x g" . magit-status)
           ("C-x M-g" . magit-dispatch)
           ("C-c M-g" . magit-file-dispatch)
           ("C-x p m" . magit-project-status))
(with-eval-after-load 'project
  (when (consp project-switch-commands)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))
(defvar magit-credential-cache-daemon-process)
(defun +magit--ccdp-no-query ()
  "Avoid query process status on exit."
  (when magit-credential-cache-daemon-process
    (set-process-query-on-exit-flag
     magit-credential-cache-daemon-process nil)))
(with-eval-after-load 'magit
  (advice-add #'magit-maybe-start-credential-cache-daemon :after '+magit--ccdp-no-query)
  (setopt magit-wip-mode-lighter "")
  (magit-wip-mode))

;;;; diff-hl

(eval-when-compile (require 'diff-hl))
(setq diff-hl-update-async t)
(autoload 'diff-hl-magit-post-refresh "diff-hl.el")
(add-hook 'tty-setup-hook #'diff-hl-margin-mode)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(with-eval-after-load 'diff-hl
  (global-diff-hl-mode))

;;;; eldoc-diffstat

(add-hook 'magit-mode-hook #'eldoc-diffstat-mode)
(add-hook 'magit-blame-mode-hook #'eldoc-diffstat-mode)
(add-hook 'vc-annotate-mode-hook #'eldoc-diffstat-mode)

;;;; eat

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
(bind-key "C-x p s" #'eat-project)
(with-eval-after-load 'project
  (define-key project-other-window-map "s" #'eat-project-other-window))
(with-eval-after-load 'project
  (when (consp project-switch-commands)
    (setq project-switch-commands
          (assq-delete-all 'project-shell
                           project-switch-commands))
    (add-to-list 'project-switch-commands
                 '(eat-project "Eat") t)))
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
(bind-key "C-c t s" '+eat/here)
(defvar eat-terminal)
(defun eat-send-pass nil
  (interactive)
  (if eat-terminal nil
    (user-error "Process not running"))
  (require 'password-store)
  (eat-term-send-string eat-terminal
                        (password-store-get
                         (password-store--completing-read
                          t)))
  (eat-self-input 1 'return))
(setq eat-kill-buffer-on-exit t)

;;;; with-editor

(bind-key "<remap> <async-shell-command>" #'with-editor-async-shell-command)
(bind-key "<remap> <shell-command>" #'with-editor-shell-command)
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

(with-eval-after-load 'orderless
  (add-to-list 'orderless-affix-dispatch-alist
               '(96 . +orderless-pinyin)))

(with-eval-after-load 'pyim
  (pyim-basedict-enable))

;;;; rime

(setq rime-disable-predicates '(meow-normal-mode-p
                                meow-keypad-mode-p
                                meow-motion-mode-p
                                meow-beacon-mode-p))
(define-advice toggle-input-method (:before (&rest _) rime)
  (setq default-input-method "rime"))

;;;; kinsoku

(setq word-wrap-by-category t)

;;;; term-keys

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

(with-eval-after-load 'term-keys
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

(setq xterm-set-window-title t)

;;;; xt-mouse

(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(with-eval-after-load 'xt-mouse
  (xterm-mouse-mode))
(if (eq (framep-on-display) t)
    (require 'xt-mouse))

;;;; clipetty

(add-hook 'tty-setup-hook #'clipetty-mode)
(with-eval-after-load 'clipetty
  (global-clipetty-mode))
(if (eq (framep-on-display) t) (require 'clipetty))

;;;; repeat

(defun +repeat--post-command ()
  (when (function-get this-command 'repeat-map)
    (message "Command %S has a `repeat-map'" this-command)
    (require 'repeat)
    (repeat-post-hook)))
(add-hook 'post-command-hook '+repeat--post-command)
(with-eval-after-load 'repeat
  (remove-hook 'post-command-hook '+repeat--post-command)
  (repeat-mode))

;;;; savehist

(autoload 'savehist-minibuffer-hook "savehist")
(add-hook 'minibuffer-setup-hook #'savehist-minibuffer-hook)
(with-eval-after-load 'savehist
  (savehist-mode))

;;;; auth-sources
(setq auth-sources '("~/.authinfo.gpg" password-store))
(with-eval-after-load 'auth-source
  (auth-source-pass-enable))

;;;; lin

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

(defun +lin-line--next-error-h ()
  (with-current-buffer next-error-buffer
    (save-selected-window
      (when-let* ((win (get-buffer-window (current-buffer))))
        (select-window win)
        (recenter))
      (when (bound-and-true-p lin-mode)
        (hl-line-highlight)))))

(with-eval-after-load 'lin
  (setopt lin-face 'lin-magenta)
  (lin-global-mode))

(add-hook 'next-error-hook '+lin-line--next-error-h)
(add-hook 'gnus-visual-mark-article-hook #'hl-line-highlight)

;;;; emacs-server

;; Workaround windows encoding issue
(defun +server--process-filter-coding-system (&rest args)
  (let ((file-name-coding-system locale-coding-system))
    (apply args)))

(with-eval-after-load 'server
  (require 'org-protocol)
  (when (memq system-type '(windows-nt ms-dos))
    (add-hook #'server-process-filter :around '+server--process-filter-coding-system))
  (unless (server-running-p)
    (server-start)))

;;;; epg

(setq epg-pinentry-mode 'loopback)
(setq epa-keys-select-method 'minibuffer)

;;;; tui

(autoload 'tui-run "tui" nil t)
(autoload 'tui-rg "tui" nil t)
(autoload 'tui-ugrep "tui" nil t)
(autoload 'tui-yazi "tui" nil t)
(autoload 'tui-kill "tui" nil t)
(autoload 'tui-line "tui" nil t)
(autoload 'tui-find "tui" nil t)
(autoload 'tui-locate "tui" nil t)

(bind-keys ("C-c t t" . tui-run)
           ("C-c t r" . tui-rg)
           ("C-c t g" . tui-ugrep)
           ("C-c t y" . tui-yazi)
           ("C-c t k" . tui-kill)
           ("C-c t l" . tui-line)
           ("C-c t f" . tui-find)
           ("C-c t d" . tui-locate))

(with-eval-after-load 'tui
  (setf (alist-get "^\\*tui-" display-buffer-alist nil nil #'equal)
        '((display-buffer-same-window))))

;;;; deadgrep

(eval-when-compile (require 'deadgrep))
(bind-key "C-c s" #'deadgrep)

;;;; gptel

(eval-when-compile (require 'gptel))
(bind-keys ("C-c t A" . gptel)
           ("C-c t a" . gptel-send))

(defvar embark-general-map)
(with-eval-after-load 'embark
  (bind-key "?" #'gptel-quick embark-general-map))

;;;; logos
(bind-keys ("<f8>" . logos-focus-mode)
           ("<remap> <narrow-to-region>" . logos-narrow-dwim)
           ("<remap> <forward-page>" . logos-forward-page-dwim)
           ("<remap> <backward-page>" . logos-backward-page-dwim))

(with-eval-after-load 'logos
  (setopt logos-outline-regexp-alist
          `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
            (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))))
  (bind-keys :map logos-focus-mode-map
             ("<left>" . logos-backward-page-dwim)
             ("<right>" . logos-forward-page-dwim)))

(defun logos-focus--narrow ()
  (when (symbol-value 'logos-focus-mode)
    (logos--narrow-to-page 0)
    (make-local-variable 'logos--restore)
    (push #'widen logos--restore)))
(setopt logos-outlines-are-pages t)
(setq-default logos-hide-cursor nil
              logos-hide-mode-line t
              logos-hide-header-line t
              logos-hide-buffer-boundaries t
              logos-hide-fringe t
              logos-buffer-read-only nil
              logos-scroll-lock nil
              logos-olivetti t)

(add-hook 'logos-focus-mode-hook #'logos-focus--narrow)

(add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

;;;; eww

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

(defun eww-reset-current-bookmark ()
  (when (and bookmark-current-bookmark
             (not (equal (eww-current-url) (bookmark-location bookmark-current-bookmark))))
    (setq bookmark-current-bookmark nil)))

(add-hook 'eww-after-render-hook 'eww+miniflux-trim)
(add-hook 'eww-after-render-hook 'eww+kagi-trim)
(with-eval-after-load 'bookmark
  (add-hook 'eww-after-render-hook 'eww-reset-current-bookmark))

;;;; bookmark

(defvar pp-default-function)
(defun +bookmark--pp-28 (&rest args)
  (let ((pp-default-function 'pp-28))
    (apply args)))

(advice-add #'bookmark-write-file :around '+bookmark--pp-28)

(setq bookmark-save-flag 1
      bookmark-watch-bookmark-file 'silent
      bookmark-version-control t)

(autoload 'url-bookmark-add "bookmark-extras.el" "" t)
(bind-key "C-x r u" #'url-bookmark-add)
(with-eval-after-load 'bookmark
  (require 'bookmark-extras))

;;;; org

(bind-keys ("C-c L" . org-store-link)
           ("C-c A" . org-agenda)
           ("C-c C" . org-capture)
           ("C-c T M" . +org/toggle-emphasis-markers)
           ("C-c T m" . org-modern-mode))

(add-hook 'org-mode-hook #'org-modern-mode)

(defun +org/toggle-emphasis-markers ()
  "Toggle the display of emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-flush))

(with-eval-after-load 'org
  (setopt org-export-backends '(html latex texinfo)))

(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(with-eval-after-load 'org-capture
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

(with-eval-after-load 'rcirc
  (rcirc-track-minor-mode))

;;;; erc

(defvar erc-modules)
(defvar erc-mode-map)
(with-eval-after-load 'erc
  ;; This enables displaying servers and channels in side windows,
  ;; which can be toggled by C-x w s.
  (setopt erc-modules
	  (seq-union '(sasl nicks scrolltobottom track)
		     erc-modules))

  ;; insert a newline when I hit <RET> at the prompt, and prefer
  ;; something more deliberate for actually send messages.
  (bind-keys :map erc-mode-map
             ("RET" . nil)
             ("C-c C-c" . erc-send-current-line))

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
  (defvar erc-fill-wrap-mode-map)
  (with-eval-after-load 'erc-fill
    (require 'erc-fill)
    (bind-keys :map erc-fill-wrap-mode-map
               ("C-c =" . erc-fill-wrap-nudge)))

  ;; prevent JOINs and PARTs from lighting up the mode-line.
  (defvar erc-track-faces-priority-list)
  (with-eval-after-load 'erc-track
    (setopt erc-track-faces-priority-list (remq 'erc-notice-face
					        erc-track-faces-priority-list)))
  (setq erc-track-priority-faces-only 'all))

;;;; gnus

(bind-key "C-c t G" #'gnus)

;;;; discourse

(add-hook 'discourse-topic-mode-hook #'visual-line-mode)
(setq discourse-debug nil)

;;;; copilot

(with-eval-after-load 'copilot
  (bind-keys :map copilot-mode-map
             ("<tab>" . copilot-accept-completion)
             ("C-<tab>" . copilot-accept-completion-by-word)))

;;;; browser-hist

(bind-key "M-s b" #'browser-hist-search)
(with-eval-after-load 'browser-hist
  (setf (alist-get 'zen browser-hist-db-paths nil t)
        (cond ((memq system-type '(cygwin windows-nt ms-dos))
               "$APPDATA/zen/Profiles/*/places.sqlite")))
  (setf (alist-get 'zen browser-hist--db-fields)
        '("title" "url" "moz_places" "ORDER BY last_visit_date desc"))
  (setopt browser-hist-default-browser 'zen))

;;;; vundo

(bind-key "C-c T u" #'vundo)
(with-eval-after-load 'vundo
  (setopt vundo-glyph-alist vundo-unicode-symbols))

;;;; markdown

(with-eval-after-load 'markdown-mode
  (bind-key "C-x C-q" #'markdown-view-mode markdown-mode-map)
  (bind-key "C-x C-q" #'markdown-mode markdown-view-mode-map)
  (bind-key "C-x C-q" #'gfm-view-mode gfm-mode-map)
  (bind-key "C-x C-q" #'gfm-mode gfm-view-mode-map))

;;;; p-search

(with-eval-after-load 'p-search
  (require 'psx-info))
(autoload 'p-search "p-search" nil t)
(bind-key "C-c t p" #'p-search)

;;;; debug

(bind-keys :prefix "C-c T d"
           :prefix-map debug-prefix-map
           ("e" . toggle-debug-on-error)
           ("q" . toggle-debug-on-quit)
           ("f" . debug-on-entry)
           ("v" . debug-on-variable-change)
           ("c f" . cancel-debug-on-entry)
           ("c v" . cancel-debug-on-variable-change))

;;;; copyright

(setq copyright-year-ranges t)
(add-hook 'before-save-hook #'copyright-update)

;;;; sftp
(autoload 'sftp "sftp" nil t)

;;;; other packages


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
  (interactive "p")
  (let ((current-alpha (frame-parameter nil 'alpha-background)))
    (when (and (numberp current-alpha) (< current-alpha 100))
      (setq +toggle-transparent-alpha current-alpha))
    (cond ((= arg 1)
           (if (and (numberp current-alpha) (< current-alpha 100))
               (modify-frame-parameters nil '((alpha-background . nil)))
             (modify-frame-parameters nil `((alpha-background . ,+toggle-transparent-alpha)))))
          ((= arg 4)
           (modify-frame-parameters nil `((alpha-background . ,+toggle-transparent-alpha))))
          (t
           (modify-frame-parameters nil `((alpha-background . nil)))))))

(bind-key "C-c f e" #'find-early-init-file)
(bind-key "C-c f i" #'find-user-init-file)

(bind-key "C-c f a" #'ffap)
(bind-key "C-c f r" #'ff-find-related-file)

(bind-key "C-c T x" #'+toggle-transparent)


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
