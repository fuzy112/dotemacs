;;; ctags-xref-c.el --- C language support for ctags-xref  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

;; Author: Zhengyi Fu  <i@fuzy.me>
;; Keywords: c, tools
;; Version: 0.6.0

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

;;; Code:

(require 'ctags-xref)
(require 'cl-seq)

(declare-function c-defun-name "cc-cmds.el")
(declare-function c-cpp-define-name "cc-cmds.el")
(declare-function c-ts-mode--emacs-current-defun-name "c-ts-mode.el")

(cl-defgeneric ctags-xref-c--defun-name ()
  (c-defun-name))

(cl-defgeneric ctags-xref-c--macro-name ()
  (c-cpp-define-name))

(cl-defmethod ctags-xref-c--defun-name (&context (major-mode c-ts-base-mode))
  (c-ts-mode--emacs-current-defun-name))

(cl-defmethod ctags-xref-c--macro-name (&context (major-mode c-ts-base-mode))
  (c-ts-mode--emacs-current-defun-name))

(defvar ctags-xref-c-annotate-score t)


(defun ctags-xref-c--filter (identifier items)
  "Filter and sort definition ITEMS of IDENTIFIER based on the context."
  (ctags-xref--message "ctags-xref-filter-tags")
  (save-excursion
    (when (not (looking-at (regexp-quote identifier)))
      (forward-symbol -1))

    (let ((is-member (looking-back "\\(?:->\\|\\.\\|::\\)[[:space:]]*" (line-beginning-position)))
          (tag-type (and (looking-back "\\(struct\\|union\\|enum\\)[[:space:]]*" (line-beginning-position))
                         (intern (match-string 1))))
          (function-scope (concat "function:" (ctags-xref-c--defun-name)))
          (macro-scope (concat "macro:" (ctags-xref-c--macro-name)))
          (class-scope (concat "class:" (and-let* ((defun-name (ctags-xref-c--defun-name)))
                                          (if (string-prefix-p "class " defun-name)
                                              (substring defun-name 6)
                                            (car-safe (split-string defun-name ":" nil "[[:space:]]"))))))
          (is-header (and (looking-at-p (concat (regexp-quote identifier) "[>\"]"))
                          (looking-back "^[[:space:]]*include[[:space:]]*[<\"]" (line-beginning-position))))
          (scores (make-hash-table :test 'eq :weakness t)))
      (ctags-xref--message "is-member %s, tag-type %s, function-scope %s, macro-scope %s, class-scope %s"
                           is-member tag-type function-scope macro-scope class-scope)

      (dolist (item items)
        (let* ((loc (xref-item-location item))
               (tag (ctags-xref-location-tag loc))
               (score 0))

          (pcase tag
            ((cl-struct ctags-xref-tag
                        (language (or "C" "C++" "QtMoc"))
                        roles kind scope)

             (pcase roles
               ("undef" (cl-decf score 5)))
             (pcase kind
               ((or "s" "struct" "g" "enum" "u" "union")
                (when (derived-mode-p '(c++-mode c++-ts-mode))
                  (cl-incf score 2))
                (when tag-type
                  (cl-incf score 2)))
               ((or "m" "member")
                (if is-member
                    (cl-incf score 1)
                  (when (equal scope class-scope)
                    (cl-incf score 2))))
               ((or "l" "local" "z" "parameter" "L" "label")
                (when (equal scope function-scope)
                  (cl-incf score 2))
                (when is-member
                  (cl-decf score 2))
                (when tag-type
                  (cl-decf score 2)))
               ((or "D" "macroparam")
                (when (equal scope macro-scope)
                  (cl-incf score 2))
                (when is-member
                  (cl-decf score 2))
                (when tag-type
                  (cl-decf score 2)))
               ((or "p" "prototype" "s" "slot" "signal")
                (if (string-prefix-p "class:" scope)
                    (if is-member
                        (cl-incf score 1)
                      (when (equal scope class-scope)
                        (cl-incf score 2)))
                  (cl-incf score 2)))
               ((or "f" "function")
                (if (string-prefix-p "class:" scope)
                    (if is-member
                        (cl-incf score 2)
                      (when (equal scope class-scope)
                        (cl-incf score 3)))
                  (cl-incf score 3)))
               ((or "x" "externvar")
                (unless (or is-member tag-type)
                  (cl-incf score 1)))
               ((or "v" "variable")
                (unless (or is-member tag-type)
                  (cl-incf score 2)))
               ((or "h" "header" "file")
                (when is-header
                  (cl-incf score 5))))))

          (puthash item score scores)))

      (setq items (seq-filter (lambda (item) (> (gethash item scores) 0)) items))

      (setq items (sort items
                        :key (lambda (item) (gethash item scores))
                        :reverse t
                        :in-place t))

      (when ctags-xref-c-annotate-score
        (dolist (item items)
          (let ((score (gethash item scores))
                (summary (xref-item-summary item)))
            (setq summary (concat summary
                                  (and score
                                       (propertize (concat "\t" (number-to-string score))
                                                   'face 'ctags-xref-score-face))))
            (setf (xref-item-summary item) summary))))

      items)))

(cl-defmethod xref-backend-definitions :around ((_backend (eql 'ctags)) identifier &context (major-mode c-mode))
  (let ((items (cl-call-next-method)))
    (ctags-xref-c--filter identifier items)))

(cl-defmethod xref-backend-definitions :around ((_backend (eql 'ctags)) identifier &context (major-mode c++-mode))
  (let ((items (cl-call-next-method)))
    (ctags-xref-c--filter identifier items)))

(cl-defmethod xref-backend-definitions :around ((_backend (eql 'ctags)) identifier &context (major-mode c-ts-base-mode))
  (let ((items (cl-call-next-method)))
    (ctags-xref-c--filter identifier items)))

(provide 'ctags-xref-c)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ctags-xref-c.el ends here
