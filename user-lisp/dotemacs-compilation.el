;;; dotemacs-compilation.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;;;; compile

(after-load! compile
  (setopt compilation-always-kill t
          compilation-ask-about-save t
          compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (setopt compilation-error-regexp-alist
          (seq-difference compilation-error-regexp-alist
                          '( absoft ada aix ant borland comma msft
                             edg-1 edg-2 epc ftnchek jikes-file jikes-line
                             cucumber lcc makepp mips-1 mips-2 oracle rxp
                             sparc-pascal-file sparc-pascal-line
                             sparc-pascal-example sun sun-ada watcom 4bsd))))

;;;; comint

(after-load! comint
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)
  (setopt comint-prompt-read-only t
          comint-buffer-maximum-size 2048
          comint-terminfo-terminal "dumb-emacs-ansi")
  (setopt comint-input-ring-size 10000
          comint-input-ignoredups t
          comint-history-isearch 'dwim
          comint-input-autoexpand t
          comint-insert-previous-argument-from-end t
          comint-buffer-maximum-size 20000
          comint-scroll-to-bottom-on-input t
          comint-move-point-for-output nil
          comint-scroll-show-maximum-output nil
          comint-pager "cat"))

;;;; grep
(after-load! grep
  (setopt grep-use-headings t))

(provide 'dotemacs-compilation)
;;; dotemacs-compilation.el ends here
