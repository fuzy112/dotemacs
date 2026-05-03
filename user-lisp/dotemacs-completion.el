;;; dotemacs-completion.el  -*- lexical-binding: t; -*-

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

;;;; orderless

(eval-when-compile (require 'dotemacs-core))
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
(setopt completion-styles '(orderless basic))

;; Let categories decide everything, do not force any extra defaults.
(setopt completion-category-defaults nil)

;; --- Category-specific tweaks ----------------------------------------
;; Different kinds of completions benefit from different matching
;; behaviour, so override the style on a per-category basis:
(setopt completion-category-overrides
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

(setq completion-pcm-leading-wildcard t)

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


;;;; Default completion UI

(setopt completions-format 'one-column
        completions-detailed t
        completions-group t
        completions-sort 'historical
        completions-max-height 15
        minibuffer-visible-completions 'up-down
        completion-eager-update t
        completion-eager-display t
        completion-auto-help 'always
        completion-show-help nil
        completion-show-inline-help nil)

(keymap-unset minibuffer-local-completion-map "SPC")

;;;; vertico

;; Allow recursive minibuffers, so commands invoked from within the minibuffer
;; (e.g., C-x C-f followed by M-:) can themselves use the minibuffer.
(setopt enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
(setopt minibuffer-depth-indicator-function
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
  (setopt vertico-quick1 "htnsd"
          vertico-quick2 "ueoai")
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode)
  (vertico-multiform-mode)
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

(after-load! marginalia
  (alist-setq! marginalia-prompt-categories
    "\\<info manuals\\>" 'info-manual
    "\\<manual name\\>" 'info-manual
    "\\<Log rev,s\\>" 'magit-rev)
  (marginalia-mode))

;;;; cursor-intangible

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Prevent typing before the prompt.
(setopt minibuffer-prompt-properties '(read-only t face minibuffer-prompt cursor-intangible t))


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
  (fmakunbound 'completion-in-region@corfu)
  (require 'orderless)
  (setopt corfu-cycle t
          corfu-preselect 'prompt)
  (setopt corfu-quick1 "htnsd"
          corfu-quick2 "ueoai")
  (global-corfu-mode)
  (corfu-history-mode)
  (define-keymap :keymap corfu-map
    "M-m"       #'corfu-move-to-minibuffer
    "SPC"       #'corfu-insert-separator
    "C-q"       #'corfu-quick-insert
    "M-q"       #'corfu-quick-complete
    "M-t"       #'corfu-popupinfo-toggle
    "TAB"       #'corfu-next
    "S-TAB"     #'corfu-previous
    "<backtab>" #'corfu-previous)

  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(after-load! corfu-popupinfo
  (keymap-unset corfu-popupinfo-map "M-t"))

(defun corfu-move-to-minibuffer ()
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (consult-completion-in-region beg end table pred)))))

;;;;; Disable corfu-auto when using input methods

(defvar +corfu-auto-saved nil)
(defun +corfu-auto-suspend ()
  (when (boundp 'corfu-auto)
    (setq-local +corfu-auto-saved corfu-auto)
    (setopt-local corfu-auto nil)))
(defun +corfu-auto-restore ()
  (if (boundp 'corfu-auto)
      (setopt-local corfu-auto +corfu-auto-saved)))

(add-hook 'input-method-activate-hook '+corfu-auto-suspend)
(add-hook 'input-method-deactivate-hook '+corfu-auto-restore)

;;;; cape

(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)
(add-hook 'completion-at-point-functions #'cape-keyword 50)

;;;; dabbrev

(defvar dabbrev-ignored-buffer-regexps)
(defvar dabbrev-ignored-buffer-modes)
(after-load! dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;;;; tempel

(defun tempel-setup-capf ()
  (setopt-local completion-at-point-functions
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
  (setopt tempel-path (concat user-emacs-directory "/templates/*.eld"))
  (add-to-list 'tempel-user-elements #'tempel-include))

(add-hook 'abbrev-mode-hook #'tempel-abbrev-mode)


;;;; embark

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

(setq prefix-help-command #'embark-prefix-help-command)

(after-load! embark
  (setopt embark-help-key "?")
  (setopt embark-cycle-key "C-.")
  (setopt embark-prompter #'embark-completing-read-prompter)
  (setopt embark-indicators '(embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
  (keymap-set embark-file-map "#" '+embark/find-file-as-root)
  (keymap-set embark-bookmark-map "W" '+embark/eww-open-bookmark)
  (keymap-set embark-bookmark-map "u" '+embark/browse-url-open-bookmark)
  (keymap-set embark-region-map "[" '+embark/apply-ansi-color))

(after-load! (:and embark vertico-multiform)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

;;;; consult

(after-load! comint
  (define-keymap :keymap comint-mode-map
    ;; "M-s" #'consult-history
    "M-r" #'consult-history))

(after-load! consult
  (remove-hook 'consult-after-jump-hook #'recenter)
  (add-hook 'consult-after-jump-hook #'reposition-window))

(setopt register-preview-delay 0.5
        register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)


;; Declare internal functions of consult to avoid bytecomp warnings.
(declare-function consult--read-1 "consult.el" (arg1 &rest rest))
(declare-function consult--file-preview "consult.el")
(declare-function consult--buffer-preview "consult.el")
(declare-function consult--file-state "consult.el")

;; Add preview for `read-file-name'.
(defun +consult--read-file-name-function (prompt &optional dir default mustmatch initial pred)
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
     :history 'file-name-history
     :default (or default "")
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
   :default       (or def "")
   :prompt        (format-prompt (replace-regexp-in-string ":[[:space:]]*\\'" "" prompt) def)
   :require-match mustmatch
   :history       'buffer-name-history
   :predicate     pred
   :preview-key   consult-preview-key
   :sort          t
   :lookup        (lambda (selected &rest _) selected)))

(setopt read-buffer-function #'+consult--read-buffer-function)

(define-advice project--completing-read-strict
    (:override (prompt collection &optional predicate hist mb-default common-parent-directory) consult)
  (cl-letf* ((mb-default (mapcar (lambda (mb-default)
                                   (if (and common-parent-directory
                                            mb-default
                                            (file-name-absolute-p mb-default))
                                       (file-relative-name
                                        mb-default common-parent-directory)
                                     mb-default))
                                 (if (listp mb-default) mb-default (list mb-default))))
             (abs-cpd (expand-file-name (or common-parent-directory "")))
             (abs-cpd-length (length abs-cpd))
             (non-essential t)          ;Avoid new Tramp connections.
             ((symbol-value hist)
              (if common-parent-directory
                  (mapcan
                   (lambda (s)
                     (setq s (expand-file-name s))
                     (and (string-prefix-p abs-cpd s)
                          (not (eq abs-cpd-length (length s)))
                          (list (substring s abs-cpd-length))))
                   (symbol-value hist))
                (symbol-value hist))))
    (require 'consult)
    (consult--read-1 collection
                     :prompt (format "%s: " prompt)
                     :predicate predicate
                     :require-match 'confirm
                     :history hist
                     :default nil
                     :add-history mb-default
                     :sort t
                     :lookup (lambda (selected &rest _) selected)
                     :state (consult--file-state)
                     :preview-key consult-preview-key)))


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
  (setopt consult-preview-key "M-.")
  (setopt consult-narrow-key "<")
  (setopt consult-widen-key ">")
  (setq-default consult--regexp-compiler #'+consult--orderless-regexp-compiler)

  ;; consult-customize is a macro and is not autoloaded
  (with-no-compile!
    (consult-customize
     consult-line
     consult-focus-lines consult-keep-lines
     consult-imenu
     consult-source-buffer
     consult-source-project-buffer
     consult-source-buffer-register
     :preview-key '(:debounce 0.2 any)
     consult-xref
     consult-ripgrep consult-grep consult-git-grep consult-ugrep
     consult-theme
     :preview-key '(:debounce 0.4 any)
     consult-source-file-register
     :preview-key '(:debounce 0.4 any)
     consult-source-recent-file
     :enabled (lambda () (require 'recentf) (default-toplevel-value 'recentf-mode))))

  (define-advice consult-recent-file (:before (&rest _) enable-recentf)
    (require 'recentf)
    (unless recentf-mode (recentf-mode)))

  ;; url-only bookmark type
  (cl-pushnew #'url-bookmark-jump (cddr (assoc ?w consult-bookmark-narrow)))

  ;; filter internal buffers
  (add-to-list 'consult-buffer-filter
               (rx (or (seq bot "*EGLOT " (+ nonl) (or "stderr" "output" "events") "*" eot)
                       (seq bot "magit-process: " (+ nonl) eot)
                       (seq bot (+ nonl) ".~" (repeat 7 (in "a-f0-9")) "~" eot)
                       (seq bot (or "*Async-Native-Compile-Log*" "nix-edit" "*envrc*"
                                    "*Compile-Log*" "*Pp Eval Output*" "*log-edit-files*"
                                    "*Messages*" "*Warnings" ".newsrc-dribble")
                            eot)))))

;;;; Prescent

(after-load! prescient
  (prescient-persist-mode))

(after-load! vertico
  ;; disable prescient's filtering since we use orderless
  (setopt vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode))

(after-load! corfu
  (setopt corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode))

(setq completion-preview-sort-function #'prescient-completion-sort)

(provide 'dotemacs-completion)
;;; dotemacs-completion.el ends here
