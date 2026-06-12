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

(orderless-define-completion-style orderless+flex
  (orderless-matching-styles '(orderless-flex)))

(orderless-define-completion-style orderless+prefixes
  (orderless-matching-styles '(orderless-prefixes
                               orderless-literal
                               orderless-regexp)))

(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))

(setopt completion-styles '(orderless basic))

(setopt completion-category-defaults nil)

(setopt completion-category-overrides
        '(
          (file        . ((styles . (partial-completion))))

          (symbol      . ((styles . (orderless+flex))))
          (symbol-help . ((styles . (orderless+flex))))

          (command     . ((styles . (orderless+initialism))))
          (variable    . ((styles . (orderless+initialism))))

          (eglot       . ((styles . (orderless))))
          (eglot-capf  . ((styles . (orderless))))

          (magit-rev   . ((styles . (partial-completion))))

          (project-file . ((styles . (orderless+prefixes))))))

(setq completion-pcm-leading-wildcard t)

;; Registers an orderless dispatcher that makes
;; 1) the “$” suffix work together with Consult’s tofu suffixes, and
;; 2) dotted file-extension patterns of the form “.EXT”.

(defun +orderless--consult-suffix ()
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$" consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

(defun +orderless--consult-dispatch (word _index _total)
  (cond
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

(after-load! orderless
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
  (define-keymap :keymap vertico-map
    ;; vertico-repeat
    "M-P"   #'vertico-repeat-previous
    "M-N"   #'vertico-repeat-next
    ;; vertico-quick
    "C-q"   #'vertico-quick-insert
    "M-q"   #'vertico-quick-exit))

(setq! vertico-multiform-commands
       '((switch-to-buffer unobtrusive)
         (man grid)
         (info-display-manual grid)
         (dired-goto-file flat)
         (ibuffer-jump-to-buffer flat)))

(setq! vertico-multiform-categories
       '((file (:keymap . vertico-directory-map))
         (embark-keybinding grid)
         (jinx grid (vertico-grid-annotate . 20) (vertico-count . 4))
         (info-manual grid)
         (magit-rev grid)))

;;;; marginalia

(autoload 'marginalia--minibuffer-setup "marginalia")
(add-hook 'minibuffer-setup-hook #'marginalia--minibuffer-setup)

(after-load! marginalia
  (alist-setq! marginalia-prompt-categories
    "\\<Log rev,s\\>" 'magit-rev
    ;; `consult-info' reads manual names without specifying category.
    "\\<info manuals\\>" 'info-manual)
  (marginalia-mode)

  (alist-delq! marginalia-command-categories recentf-open)

  (define-advice marginalia--annotator (:override (cat) cat-inherit)
    "Return annotation function for category CAT."
    (pcase (car (alist-get cat marginalia-annotators))
      ('none #'ignore)
      ('builtin nil)
      ('nil (cl-loop for p in (get cat 'completion-category-parents)
                     thereis (marginalia--annotator p)))
      (fun fun))))

;; Marginalia and embark expects recentf to be files.
(define-completion-category 'recentf '(file)
  "Completion category for `recentf'."
  :styles '(flex)
  :cycle-sort-function #'identity
  :display-sort-function #'identity)

;; The following completion categories are used by marginalia and are
;; not built-in to Emacs.  Let them inherit from built-in categories.

(define-completion-category 'symbol '(symbol-help)
  "Completion category for symbols.")

(define-completion-category 'function '(symbol)
  "Completion category for functions.")

(define-completion-category 'command '(function)
  "Completion category for commands.")

(define-completion-category 'variable '(symbol)
  "Completion category for variables.")

(define-completion-category 'minor-mode '(function)
  "Completion category for minor modes.")

(define-completion-category 'face '(symbol)
  "Completion category for Faces.")

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

(declare-function tempel--templates "ext:tempel.el")
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
    (dlet ((ansi-color-apply-face-function
            (if use-overlays #'ansi-color-apply-overlay-face
              #'ansi-color-apply-text-property-face)))
      (ansi-color-apply-on-region beg end))))

(defun embark-inject (str)
  "Replace the contents of the active minibuffer with STR.
If there is no active minibuffer, signal an error."
  (let ((win (active-minibuffer-window)))
    (unless win
      (user-error "No active minibuffer"))
    (select-window win)
    (delete-minibuffer-contents)
    (insert str)))

(defun embark-inject-variable-value (var)
  "Replace the minibuffer contents with the value of VAR."
  (interactive "SVariable: ")
  (embark-inject (string-trim (pp-to-string (symbol-value var)))))

(defun +embark/search-web (query)
  (interactive "sQuery: ")
  (browse-url (format "https://kagi.com/search?q=%s" query)))

(setq! prefix-help-command #'embark-prefix-help-command)

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (require 'which-key)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately."
  (require 'which-key)
  (which-key--hide-popup-ignore-command)
  (defvar embark-indicators)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(defun embark-kmacro-target ()
  "Target a textual kmacro in braces."
  (save-excursion
    (let ((beg (progn (skip-chars-backward "^{}\n") (point)))
          (end (progn (skip-chars-forward "^{}\n") (point))))
      (when (and (eq (char-before beg) ?{) (eq (char-after end) ?}))
        `(kmacro ,(buffer-substring-no-properties beg end)
                 . (,(1- beg) . ,(1+ end)))))))

(defun embark-kmacro-run (arg kmacro)
  (interactive "p\nsKmacro: ")
  (kmacro-call-macro arg t nil (kbd kmacro)))

(defun embark-kmacro-save (kmacro)
  (interactive "sKmacro: ")
  (kmacro-push-ring)
  (setq last-kbd-macro (kbd kmacro)))

(defun embark-kmacro-name (kmacro name)
  (interactive "sKmacro: \nSName: ")
  (let ((last-kbd-macro (kbd kmacro)))
    (kmacro-name-last-macro name)))

(defun embark-kmacro-bind (kmacro)
  (interactive "sKmacro: \n")
  (let ((last-kbd-macro (kbd kmacro)))
    (kmacro-bind-to-key nil)))

(after-load! embark
  (setopt embark-help-key "C-h")
  (setopt embark-cycle-key "C-.")
  (setopt embark-prompter #'embark-keymap-prompter)
  (setopt embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
  (add-to-list 'embark-target-finders 'embark-kmacro-target)

  (defvar-keymap embark-kmacro-map
    :doc "Actions on kmacros."
    :parent embark-general-map
    "RET" #'embark-kmacro-run
    "s" #'embark-kmacro-save
    "n" #'embark-kmacro-name
    "b" #'embark-kmacro-bind)

  (alist-setq! embark-keymap-alist
    kmacro '(embark-kmacro-map))

  (alist-setq! embark-exporters-alist
    consult-location #'embark-consult-export-location-grep)
  (keymap-set embark-general-map "J" #'embark-inject)
  (keymap-set embark-general-map "/" #'embark-history-remove)
  (keymap-set embark-general-map "W" `("Search web" . ,#'+embark/search-web))
  (keymap-set embark-file-map "#" '+embark/find-file-as-root)
  (keymap-set embark-file-map "r" 'find-file-read-only)
  (keymap-set embark-file-map "V" 'view-file)
  (keymap-set embark-bookmark-map "W" '+embark/eww-open-bookmark)
  (keymap-set embark-bookmark-map "u" '+embark/browse-url-open-bookmark)
  (keymap-set embark-region-map "[" '+embark/apply-ansi-color)
  (keymap-set embark-variable-map "I" #'embark-inject-variable-value))

(defvar embark-keymap-alist)

(defun embark--raw-action-keymap-symbols (type)
  (or (alist-get type embark-keymap-alist)
      (cl-loop for p in (get type 'completion-category-parents)
               append (alist-get p embark-keymap-alist))))

(define-advice embark--raw-action-keymap (:override (type) cat-inherit)
  "Return raw action map for targets of given TYPE.
This does not take into account the default action, help key or
cycling bindings, just what's registered in
`embark-keymap-alist'."
  (make-composed-keymap
   (mapcar #'symbol-value
           (let ((actions (or (embark--raw-action-keymap-symbols type)
                              (alist-get t embark-keymap-alist))))
             (ensure-list actions)))))

;;;; consult

(after-load! comint
  (define-keymap :keymap comint-mode-map
    "M-h" #'cape-history
    "M-r" #'consult-history))

(after-load! consult
  (alist-setq! consult-mode-histories
    eat-mode '(eat--line-input-ring eat--line-input-ring-index beginning-of-line))
  (add-hook 'consult-after-jump-hook #'pulse-momentary-highlight-one-line))

(setopt register-preview-delay 0.5)
(setq! register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)


;; Declare internal functions of consult to avoid bytecomp warnings.
(declare-function consult--read-1 "ext:consult.el" (arg1 &rest rest))
(declare-function consult--file-preview "ext:consult.el")
(declare-function consult--buffer-preview "ext:consult.el")
(declare-function consult--file-state "ext:consult.el")

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

(setq! read-file-name-function #'+consult--read-file-name-function)

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

(setq! read-buffer-function #'+consult--read-buffer-function)

(defun +project-prompt-project-dir (&optional prompt predicate require-known)
  "Prompt the user for a directory that is one of the known project roots.
This function is the same as `project-prompt-project-dir' (which see)
except that it specifies `identity' as the `display-sort-function' and
`cycle-sort-function' in the completion table."
  (defvar project-prune-zombie-projects)
  (defvar project--list)
  (defvar project--dir-history)
  (project--ensure-read-project-list)
  (when-let* ((pred (alist-get 'prompt project-prune-zombie-projects))
              (inhibit-message t))
    (project--delete-zombie-projects pred))
  (let* ((dir-choice "... (choose a dir)")
         (current (and-let* ((p (project-current))
                             (_ (or (null predicate)
                                    (funcall predicate
                                             (project-root p)))))
                    (project-root p)))
         (choices
          (let ((table (if require-known project--list
                         (append project--list `(,dir-choice)))))
            (lambda (string pred action)
              (cond
               ((eq action 'metadata)
                `(metadata . ((category . project-file)
                              (display-sort-function . identity)
                              (cycle-sort-function . identity))))
               (t
                (complete-with-action action table string pred))))))
         (project--dir-history (project-known-project-roots))
         (pr-dir ""))
    (while (string-empty-p pr-dir)
      ;; If the user simply pressed RET (and CURRENT is nil), do this
      ;; again until they don't.
      (setq pr-dir
            (let (history-add-new-input)
              (completing-read
               (format-prompt "%s" current (or prompt "Select project"))
               choices (and predicate
                            (lambda (choice)
                              (or (equal choice dir-choice)
                                  (funcall predicate choice))))
               t nil 'project--dir-history current))))
    (if (equal pr-dir dir-choice)
        (read-directory-name "Select directory: " default-directory nil t)
      pr-dir)))

(setopt project-prompter #'+project-prompt-project-dir)

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
             ;; FIXME: minibuffer-completing-file-name is a variable
             ;; used internally in minibuf.c and it may be obsolete in
             ;; the future.
             (minibuffer-completing-file-name t)
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
                     :state (consult--file-preview)
                     :preview-key consult-preview-key)))


;; Use `orderless-compile' as the `consult''s default regexp compiler.

(declare-function orderless-compile "ext:orderless.el" (arg1 &optional arg2 arg3))
(declare-function orderless--highlight "ext:orderless.el" (arg1 arg2 arg3))
(declare-function consult--convert-regexp "ext:consult.el" (arg1 arg2))

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
  (setq! consult--regexp-compiler #'+consult--orderless-regexp-compiler)

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
                       (seq bot (or "*Async-native-compile-log*" "nix-edit" "*envrc*"
                                    "*Compile-Log*" "*Pp Eval Output*" "*log-edit-files*"
                                    "*Messages*" "*Warnings" ".newsrc-dribble")
                            eot)))))

;;;; Prescent

(after-load! prescient
  (prescient-persist-mode))

(define-advice vertico-prescient--remember-minibuffer-contents (:around (orig) password)
  (unless (bound-and-true-p read-passwd-mode)
    (funcall orig)))

(after-load! vertico
  ;; disable prescient's filtering since we use orderless
  (setopt vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode))

(after-load! corfu
  (setopt corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode))

(setq! completion-preview-sort-function #'prescient-completion-sort)

(provide 'dotemacs-completion)
;;; dotemacs-completion.el ends here
