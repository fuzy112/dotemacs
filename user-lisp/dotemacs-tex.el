;;; dotemacs-tex.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;;;; TeX

(after-load! auctex
  (setopt TeX-auto-save t)
  (setopt TeX-parse-self t)
  (setopt TeX-master nil))

(add-hook 'LaTeX-mode-hook #'turn-on-reftex) ; AUCTeX
(add-hook 'latex-mode-hook #'turn-on-reftex) ; Emacs latex mode

(provide 'dotemacs-tex)
;;; dotemacs-tex.el ends here
