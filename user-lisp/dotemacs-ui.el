;;; dotemacs-ui.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;; mouse

(setopt context-menu-mode t
        mouse-yank-at-point t
        mouse-drag-and-drop-region t
        mouse-drag-and-drop-region-cross-program t
        mouse-drag-mode-line-buffer t)


;;;; nerd-icons

(add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(add-hook 'marginalia-mode-hook    #'nerd-icons-completion-mode)
(add-hook 'ibuffer-mode-hook       #'nerd-icons-ibuffer-mode)
(add-hook 'dired-mode-hook         #'nerd-icons-multimodal-mode)
(add-hook 'archive-mode-hook       #'nerd-icons-multimodal-mode)
(add-hook 'tar-mode-hook           #'nerd-icons-multimodal-mode)
(add-hook 'grep-mode-hook          #'nerd-icons-grep-mode)

(after-load! (:and nerd-icons-multimodal deb-view)
  (add-hook 'debview-mode-hook #'nerd-icons-multimodal-refresh)
  (pcase-dolist (`(,_func . ,alist) nerd-icons-multimodal-functions-alist)
    (setf (alist-get 'debview-mode alist) (alist-get 'tar-mode alist))))

;; show nerd-icons on mode-line
(setq-default mode-line-buffer-identification
              (seq-union '((:eval (nerd-icons-icon-for-buffer)) " ")
                         mode-line-buffer-identification))

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

(setopt tab-bar-tab-name-format-function
        (lambda (tab i)
          (tab-bar-tab-name-format-face
           (concat " "
                   (propertize (number-to-string i) 'face '(:weight ultra-bold :underline t))
                   " "
                   (alist-get 'name tab)
                   " ")
           tab i)))
(add-hook 'tab-bar-mode-hook #'tab-bar-history-mode)

;;; side window

(defun +toggle-side-window (side &optional frame)
  "Toggle a side window on SIDE of FRAME.
When a side window exists on SIDE, close it and remember its state.
When no side window exists on SIDE, restore the remembered state.

SIDE is one of the symbols `left', `right', `above', or `below'.
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

;;;; Display tables

;; Set up display table to use unicode chars to display frame borders
;; in terminal

(run-after-init #'standard-display-unicode-special-glyphs)

;;;; hl-line

;; Highlight current line in the error buffer after running `next-error'.
(declare-function hl-line-highlight "hl-line.el")
(defun +lin-line--next-error-h ()
  "Highlight the current line in the error buffer."
  (save-selected-window
    (when-let* ((win (get-buffer-window next-error-buffer)))
      (select-window win)
      (recenter))
    (when (bound-and-true-p hl-line-mode)
      (hl-line-highlight))))

(after-load! hl-line
  (add-hook 'next-error-hook '+lin-line--next-error-h))

;;;; lin

;; Stylistic enhancement for hl-line for selection based UI.

(dolist (hook '(archive-mode-hook
                dired-mode-hook
                dired-mode-hook
                ement-room-list-mode-hook
                ement-notifications-mode-hook
                git-rebase-mode-hook
                gnus-group-mode-hook
                gnus-summary-mode-hook
                grep-mode-hook
                ibuffer-mode-hook
                log-view-mode-hook
                logview-mode-hook
                magit-mode-hook
                occur-mode-hook
                org-agenda-mode-hook
                proced-mode-hook
                tabulated-list-mode-hook
                tar-mode-hook
                vc-dir-mode-hook
                world-clock-mode-hook
                xref--xref-buffer-mode-hook))
  (add-hook hook #'lin-mode))

;;;; prism

(declare-function prism-blend "prism.el")
(declare-function prism-set-colors "prism.el")

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
   :desaturations (number-sequence 0 37.5 2.5)
   ;; Create a series of lightness values from 0 to 37.5 in steps of 2.5
   :lightens (number-sequence 0 37.5 2.5)
   ;; Choose colors based on the active theme
   :colors (cond
            ;; For Modus themes, use theme-specific colors
            ((and (fboundp 'modus-themes-get-current-theme)
                  (modus-themes-get-current-theme))
             (with-no-compile!
               (modus-themes-with-colors
                 (list pink fg-alt green indigo))))
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


;;;; mode-line

(after-load! doom-modeline
  (setopt doom-modeline-gnus t
          doom-modeline-irc t
          doom-modeline-modal-icon doom-modeline-icon))
(after-init!
  (doom-modeline-mode))

;;; message-ring

(message-ring-mode)


;;;; ibuffer

;; Replace `list-buffers' with `ibuffer-jump'.
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-jump)

(after-load! ibuffer
  (setopt ibuffer-expert t
          ibuffer-show-empty-filter-groups nil
          ibuffer-default-sorting-mode 'filename/process
          ibuffer-use-header-line t
          ibuffer-default-shrink-to-minimum-size nil)

  (setopt ibuffer-saved-filter-groups
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
                       (mode . magit-status-mode)
                       (mode . magit-stash-mode)))
             ("Apps" (or
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
                       (name . "\\*Process List\\*")))))))

(declare-function ibuffer-switch-to-saved-filter-groups "ibuf-ext.el")

(defun ibuffer-config-saved-filter-groups ()
  (ibuffer-switch-to-saved-filter-groups "Main"))

(add-hook 'ibuffer-mode-hook #'ibuffer-config-saved-filter-groups)

;;;; Goggles
(add-hook 'prog-mode-hook #'goggles-mode)
(add-hook 'text-mode-hook #'goggles-mode)

;;;; breadcrumb

;; Enable breadcrumb-local-mode in various major modes
(add-hook 'text-mode-hook #'breadcrumb-local-mode)    ; Enable for text files
(add-hook 'conf-mode-hook #'breadcrumb-local-mode)    ; Enable for configuration files
(add-hook 'prog-mode-hook #'breadcrumb-local-mode)    ; Enable for programming modes


(defun smart-kill-buffer ()
  (interactive)
  (if current-prefix-arg
      (call-interactively #'kill-buffer)
    (call-interactively #'kill-current-buffer)))

(provide 'dotemacs-ui)
;;; dotemacs-ui.el ends here
