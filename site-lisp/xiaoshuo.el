;;; xiaoshuo.el --- Major mode for reading novels -*- lexical-binding: t -*-
;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "30.1"))
;; Version: 0.1.0
;; Keywords: text

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

;;; Commentary:
;;; Code:

(defvar xiaoshuo-heading-regexp
  "第\\(\\(?:一\\|二\\|三\\|四\\|五\\|六\\|七\\|八\\|九\\|十\\|百\\|千\\|万\\)+\\)\\(章\\|节\\|回\\|部\\|卷\\)")

;; https://emacs.stackexchange.com/questions/70326/how-can-i-apply-a-user-defined-fontset-to-a-face
;; You can apply a fontset to a face by setting its :fontset attribute
;; with set-face-attribute.  The face will then use all fonts in the
;; set as expected.  However, you cannot set the :fontset attribute
;; using custom-theme-set-faces or defface, which ignore that
;; attribute.  The manual currently says in some places that you can
;; pass in a fontset for the :font attribute or the font argument of
;; set-face-font, but this is misleading at best: the fontset will be
;; stripped of the "set" part and be treated as just an ASCII font.

(defface xiaoshuo-content
  '((t :family "LXGW WenKai" :height 120))
  "Face for displaying XiaoShuo.")

(defun xiaoshuo--outline-level ()
  (and (looking-at xiaoshuo-heading-regexp)
       (assoc-default (match-string 2)
                      '(("章" . 3)
                        ("节" . 4)
                        ("回" . 3)
                        ("部" . 1)
                        ("卷" . 2))
                      #'equal
                      '(nil . 3))))

;;;###autoload
(define-derived-mode xiaoshuo-mode text-mode "XiaoShuo"
  (create-fontset-from-ascii-font "DejaVu Serif" nil "xiaoshuo")
  (set-fontset-font "fontset-xiaoshuo" 'han "LXGW WenKai")
  (set-fontset-font "fontset-xiaoshuo" 'cjk-misc "LXGW WenKai")
  (set-face-attribute 'xiaoshuo-content nil :fontset "fontset-xiaoshuo")
  (visual-line-mode)
  (view-mode)
  (hi-lock-face-buffer xiaoshuo-heading-regexp 'bold)
  (hi-lock-mode)
  (setq-local buffer-face-mode-face 'xiaoshuo-content)
  (buffer-face-mode)
  (setq-local outline-regexp (concat "^" xiaoshuo-heading-regexp))
  (setq-local outline-level #'xiaoshuo--outline-level)
  (outline-minor-mode)
  (require 'kinsoku))

(provide 'xiaoshuo)
;;; xiaoshuo.el ends here
