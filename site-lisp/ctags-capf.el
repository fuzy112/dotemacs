;;; ctags-capf.el --- CAPF for Universal-Ctags       -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: c, tools
;; Version: 0.1.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; This Emacs package provides a completion-at-point function using
;; Universal-Ctags.

;;; Code:


(require 'ctags-core)

(ctags-define-tag ctags-capf-tag
  name input line pattern content typeref signature kind)

(defun ctags-capf--get-candidates (buf new-input)
  (mapcar (lambda (tag)
            (let ((name (ctags-capf-tag-name tag)))
              (add-text-properties 0 (length name) `(ctags-capf-tag ,tag) name)
              name))
          (with-current-buffer buf
            (ctags-capf-tag-readtags "-Ene" "-p" "-" new-input))))

;;;###autoload
(defun ctags-capf--table (beg end)
  (let ((input 'init)
        (beg (copy-marker beg))
        (end (copy-marker end))
        (table)
        (buf (current-buffer)))
    (lambda (str pred action)
      (unless (or (eq action 'metadata) (eq (car-safe action) 'boundaries))
        (let ((new-input (with-current-buffer buf (buffer-substring-no-properties beg end))))
          (when (or (eq input 'init)
                    (not (or (string-match "\\s-" new-input)
                             (string-search input new-input))))
            (setq table (ctags-capf--get-candidates buf new-input)
                  input new-input)))
        (complete-with-action action table str pred)))))

(defun ctags-capf--annotate (cand)
  (let* ((tag (get-text-property 0 'ctags-capf-tag cand))
         (sig  (ctags-capf-tag-signature tag))
         (typeref  (ctags-capf-tag-typeref tag)))
    (or typeref
        sig)))

(defun ctags-capf--company-kind (cand)
  (let* ((tag (get-text-property 0 'ctags-capf-tag cand))
         (kind (ctags-capf-tag-kind tag)))
    (pcase kind
      ("prototype" 'function)
      ("group" 'file)
      ("target" 'file)
      ("externvar" 'variable)
      ("localVariable" 'variable)
      ("unknown" 'text)
      ("const" 'constant)
      ("custom" 'variable)
      ("alias" 'function)
      ("definition" 'text)
      (_ 'unknown))))

;;;###autoload
(progn
  (defun ctags-capf (&optional interactive)
    "CAPF function for ctags utilizing `readtags (1)'."
    (interactive "p")
    (if interactive
        (let ((completion-at-point-functions '(ctags-capf)))
          (completion-at-point))
      (when (ctags-tags-file-path)
        (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'symbol)))
          (list beg end
                (ctags-capf--table beg end)
                :annotation-function #'ctags-capf--annotate
                :company-kind #'ctags-capf--company-kind
                :exclusive 'no))))))

(provide 'ctags-capf)
;;; ctags-capf.el ends here
