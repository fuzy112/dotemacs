;;; dotemacs-text.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;;;; text-mode

(defun +text-mode--capf ()
  "Addd `cape-dict' to the buffer local value of `completion-at-point-functions'."
  (when (fboundp 'cape-dict)
    (add-hook 'completion-at-point-functions #'cape-dict 90 t)))
(add-hook 'text-mode-hook #'+text-mode--capf)


;;;; nxml-mode

(defun +nxml-mode--flymake ()
  (when (fboundp 'flymake-xmllint)
    (add-hook 'flymake-diagnostic-functions #'flymake-xmllint nil t)))
(add-hook 'nxml-mode-hook #'+nxml-mode--flymake)

;;;; yaml

(add-to-list 'auto-mode-alist '("/\\.clang\\(?:d\\|-format\\)\\'" . yaml-mode))

;;;; cmake

(add-hook 'cmake-mode-hook #'cmake-capf-setup)
(add-hook 'cmake-ts-mode-hook #'cmake-capf-setup)

;;;; qmake

(alist-setq! auto-mode-alist
  "\\.pr[oi]\\'" 'makefile-mode)

;;;; systemd-mode

(defun +systemd-mode--setup ()
  (flymake-mode 1)
  (when (fboundp 'flymake-systemd)
    (add-hook 'flymake-diagnostic-functions #'flymake-systemd nil t)))

(add-hook 'systemd-mode-hook #'+systemd-mode--setup)

(provide 'dotemacs-text)
;;; dotemacs-text.el ends here
