;;; dotemacs-eshell.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

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

(provide 'dotemacs-eshell)
;;; dotemacs-eshell.el ends here
