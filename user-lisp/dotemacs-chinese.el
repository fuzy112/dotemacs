;;; dotemacs-chinese.el  -*- lexical-binding: t; -*-

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
