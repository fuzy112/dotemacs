;;; dotemacs-editor.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;;;; indent-aux

;; New minor mode in Emacs 30.1: deindents code copied into kill-ring.
(when (fboundp 'kill-ring-deindent-mode)
  (kill-ring-deindent-mode))


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

(defun turn-on-whitespace-mode-for-prog-mode ()
  (setopt-local whitespace-style '( face trailing empty indentation
                                    space-before-tab space-after-tab
                                    missing-newline-at-eof))
  (whitespace-mode))

(dolist (hook '(prog-mode-hook conf-mode-hook yaml-mode-hook))
  (add-hook hook #'turn-on-whitespace-mode-for-prog-mode))


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
;; (setq recentf-save-file (locate-user-emacs-file '("recentf.eld.zst")))

(autoload 'recentf-track-opened-file "recentf"
  "Insert the name of the file just opened or written into the recent list." )
(add-hook 'find-file-hook #'recentf-track-opened-file)
;; (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)
(after-load! recentf
  (setopt recentf-max-saved-items 8192)
  (let ((inhibit-message t))
    (recentf-mode)))

;;;; saveplace

;; (setq save-place-file
;;       (locate-user-emacs-file '("places.eld.zst")))

(autoload 'save-place-find-file-hook "saveplace")
(autoload 'save-place-dired-hook "saveplace")
(add-hook 'find-file-hook #'save-place-find-file-hook)
(add-hook 'dired-initial-position-hook #'save-place-dired-hook)
(after-load! saveplace
  (setopt save-place-limit 65536)
  (save-place-mode))

(define-advice save-place-find-file-hook (:after (&rest _) recenter)
  (when buffer-file-name
    (ignore-errors
      (reposition-window))))

;;;; savehist

(after-init!
  (setopt savehist-ignored-variables '(buffer-name-history))
  (savehist-mode))

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


;;;; autorevert

(autoload 'auto-revert--global-adopt-current-buffer "autorevert")
(add-hook 'find-file-hook #'auto-revert--global-adopt-current-buffer)

(after-load! autorevert
  (global-auto-revert-mode))


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
  (setopt visual-fill-column-center-text (symbol-value 'visual-fill-column-mode)))

;;;; hl-todo

(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'conf-mode-hook #'hl-todo-mode)

(after-load! hl-todo
  (define-keymap :keymap hl-todo-mode-map
    "M-g C-t [" #'hl-todo-previous
    "M-g C-t ]" #'hl-todo-next
    "M-g C-t o" #'hl-todo-occur
    "M-g C-t g" #'hl-todo-rgrep
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
(after-load! display-line-numbers
  (setopt display-line-numbers-type 'relative))

;;;; elec-pair

(electric-pair-mode)
(after-load! elec-pair
  (keymap-set electric-pair-mode-map "]" #'up-list))

;;;; Re-builder

(after-load! re-builder
  (setopt reb-re-syntax 'string))


;;;; undo-fu-session

(setopt undo-fu-session-compression 'zst)
(undo-fu-session-global-mode)

;;;; vundo

(after-load! vundo
  (setopt vundo-glyph-alist vundo-unicode-symbols))

;;;; copyright

(setq-default copyright-year-ranges t)

(defun +copyright-update ()
  "Update copyright year in the current buffer and save it.
This function calls `copyright-update' to update the copyright notice,
then saves the buffer.  It skips processing in `diff-mode' and `log-edit-mode'."
  (unless (derived-mode-p '(diff-mode log-edit-mode))
    (copyright-update)
    (save-buffer)))

(add-hook 'after-save-hook #'+copyright-update)



(defun clear-text-properties-in-region (beg end)
  "Clear all text properties in the selected region.
BEG and END specify the region boundaries."
  (interactive "r")
  (when (use-region-p)
    (let ((inhibit-read-only t))
      (set-text-properties beg end nil)
      (message "Cleared all text properties in region %d-%d" beg end))))



(provide 'dotemacs-editor)
;;; dotemacs-editor.el ends here
