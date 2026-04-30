;;; dotemacs-chinese.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;;;; pyim

(declare-function pyim-cregexp-build "pyim-cregexp.el")
(defun +orderless-pinyin (component)
  (require 'pyim)
  (pyim-cregexp-build component 3 t))

(after-load! orderless
  ;; 通过拼音搜索中文
  (alist-setq! orderless-affix-dispatch-alist ?` #'+orderless-pinyin))

(after-load! pyim
  (pyim-basedict-enable))

;;;; rime

(after-load! rime
  (setopt rime-disable-predicates
          '(meow-normal-mode-p
            ;; meow-keypad-mode-p
            ;; meow-motion-mode-p
            ;; meow-beacon-mode-p
            rime-predicate-prog-in-code-p
            rime-predicate-after-ascii-char-p))
  (setopt rime-inline-predicates
          '(rime-predicate-space-after-cc-p)))

(setopt default-input-method "rime")

;;;; kinsoku

(setopt word-wrap-by-category t)

(provide 'dotemacs-chinese)
;;; dotemacs-chinese.el ends here
