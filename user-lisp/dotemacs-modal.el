;;; dotemacs-modal.el  -*- lexical-binding: t; -*-

;;;; meow-edit

(eval-when-compile (require 'dotemacs-core))
(require 'meow)

(defun meow-setup ()
  "Setup meow keybindings for Dvorak layout."
  (setopt meow-keypad-leader-dispatch "C-c")
  (setopt meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (setopt meow-use-clipboard t)
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
  (meow-motion-define-key '("<escape>" . ignore))
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
   '("q" . quit-window)
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
   '("}" . meow-forward-barf)
   '("`" . meow-universal-argument)))

(define-advice meow--set-cursor-type (:override (type) terminal)
  ;; On terminals meow tries to set cursor type with escape sequences
  ;; which may conflict with a recent change in Emacs core, where
  ;; automatic cursor type change is implemented.
  (setq cursor-type type))

(define-advice meow--set-cursor-color (:after (face) terminal)
  (set-face-background 'cursor (face-background face)))

(meow-setup)
;; (alist-setq! meow-replace-state-name-list
;;   normal "🅝"
;;   motion "🅼"
;;   keypad "🅺"
;;   beacon "🅑"
;;   insert "🅘")
;; (defun +meow-setup-indicator ()
;;   (when meow-global-mode
;;     (meow-setup-indicator)))
;; (add-hook 'meow-global-mode-hook #'+meow-setup-indicator)
(meow-global-mode)

(when (treesit-available-p)
  (require 'meow-tree-sitter)
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

(provide 'dotemacs-modal)
;;; dotemacs-modal.el ends here
