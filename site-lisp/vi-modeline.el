;;; vi-modeline.el --- Vi-compatible modeline -*- lexical-binding: t; -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.3
;; Keywords: emulations

;; This file is not part of GNU Emacs.

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

;; This package defines a global minor mode that recognizes some vi variables
;; in vi-style modeline magic.

;;; Code:

(defgroup vi-modeline nil
  "Vi modeline."
  :group 'emulations
  :prefix "vi-modeline-")

(defcustom vi-modeline-known-vars-alist
  '((nil . ((ts . tab-width)
            (tabstop . tab-with)))
    (c-mode . ((sw . c-file-offset)
               (shiftwidth . c-file-offset)))
    (c++-mode . ((sw . c-file-offset)
                 (shiftwidth . c-file-offset)))
    (cperl-mode . ((sw . cperl-indent-level)
                   (shiftwidth . c-file-offset)))
    (perl-mode . ((sw . perl-indent-level)
                  (shiftwidth . c-file-offset)))
    (sh-mode . ((sw . sh-basic-offset)
                (shiftwidth . c-file-offset))))
  "Known vi variables."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol
                                   :value-type symbol)))


(defvar-local vi-modeline-var-count 0)

(defvar-local vi-modeline-format "")

;;;###autoload
(define-minor-mode vi-modeline-mode
  "A global minor mode that supports Vi-style modeline magic."
  :global t
  (if vi-modeline-mode
      (add-hook 'hack-local-variables-hook #'vi-modeline-apply)
    (remove-hook 'hack-local-variables-hook #'vi-modeline-apply)))

(setf (alist-get 'vi-modeline-mode minor-mode-alist)
      '(vi-modeline-format))

(defun vi-modeline-update ()
  "Update modeline for the current buffer."
  (setq vi-modeline-format
        (if (> vi-modeline-var-count 0)
            (format " VI[%i]" vi-modeline-var-count)
          "")))

(defun vi-modeline-apply ()
  "Find and apply vi modelines in the current buffer."
  ;; (message "vi-modeline-appy")
  (when vi-modeline-mode
    (save-excursion
      (goto-char (point-min))
      (setq vi-modeline-var-count 0)
      (when (re-search-forward "\\(#\\|//\\|/\\*\\|;;\\).* vim?:" nil t)
        (save-restriction
          (narrow-to-region (point) (line-end-position))
          (while (re-search-forward "\\_<\\([[:alpha:]]+\\)=\\([[:digit:]]+\\|\"[^\"]*\"\\)\\_>" nil t)
            ;; (message "%s=%s" (match-string 1) (match-string 2))
            (let ((key (read (match-string 1)))
                  (value (read (match-string 2))))
              (message "%S" (cons key value))
              (pcase-dolist (`(,mode . ,vars) vi-modeline-known-vars-alist)
                (when (or (null mode) (derived-mode-p mode))
                  (when-let* ((el-var (alist-get key vars)))
                    ;; (message "mode: %S key: %S el-var: %S value: %S" mode key el-var value)
                    (setf (alist-get el-var file-local-variables-alist) value)
                    (cl-incf vi-modeline-var-count)))))))))
    (vi-modeline-update)))

(provide 'vi-modeline)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; vi-modeline.el ends here
