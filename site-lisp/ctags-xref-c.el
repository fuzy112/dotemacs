;;; ctags-xref-c.el --- C language support for ctags-xref  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

;; Author: Zhengyi Fu  <i@fuzy.me>
;; Keywords: c, tools
;; Version: 0.4.0

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

;; TODO: improve readability.

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


(defun ctags-xref--c-sort (items)
  (let ((is-function-call
         (save-excursion
           (when-let ((symbol (thing-at-point 'symbol)))
             (when (not (looking-back (regexp-opt `(,symbol) 'symbols) (line-beginning-position)))
               (forward-symbol 1))
             (looking-at-p "[[:space:]]*(")))))
    (cl-stable-sort
     items
     #'>
     :key #'(lambda (item)
              (let ((tag (ctags-xref-location-tag (xref-item-location item)))
                    (score 0))
                (pcase-let (((cl-struct ctags-xref-tag
                                        input kind)
                             tag))
                  (when (equal input (buffer-file-name))
                    (cl-incf score))
                  (when (and (buffer-file-name)
                             (equal (file-name-nondirectory input)
                                    (file-name-nondirectory (buffer-file-name))))
                    (cl-incf score))
                  (when (and is-function-call (member kind '("f" "function" "p" "prototype" "s" "signal" "slot" "d" "macro")))
                    (cl-incf score))
                  (when (member kind '("m" "macro"))
                    (cl-incf score)))
                (ctags-xref--message "score %d -- %s" score tag)
                score)))))


(defun ctags-xref-c--filter (identifier items)
  "Filter TAGS of IDENTIFIER based on the context."
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
          protos fns
          externvars vars)
      (ctags-xref--message "is-member %s, tag-type %s, function-scope %s, macro-scope %s, class-scope %s"
                           is-member tag-type function-scope macro-scope class-scope)
      (let ((results
             (cl-delete-if-not
              (lambda (item)
                (ctags-xref--message "filtering item %s..." item)
                (let* ((tag (ctags-xref-location-tag (xref-item-location item)))
                       (res
                        (pcase-exhaustive tag
                          ((cl-struct ctags-xref-tag (roles "undef") (language (or "C" "C++")))
                           nil)
                          ((cl-struct ctags-xref-tag (kind (or "s" "struct" "g" "enum" "u" "union")) (language (or "C" "C++")))
                           (or (derived-mode-p '(c++-mode c++-ts-mode))
                               tag-type))
                          ((cl-struct ctags-xref-tag (kind (or "m" "member")) (language (or "C" "C++")) scope)
                           (or is-member
                               (equal scope class-scope)))
                          ((cl-struct ctags-xref-tag (kind (or "l" "local" "z" "parameter" "L" "label")) (language (or "C" "C++")) scope)
                           (equal scope function-scope))
                          ((cl-struct ctags-xref-tag (kind (or "D" "macroparam")) (language (or "C" "C++")) scope)
                           (equal scope macro-scope))
                          ((cl-struct ctags-xref-tag (kind (or "p" "prototype"  "s" "slot" "s" "signal")) (language (or "C" "C++" "QtMoc")) scope)
                           (and (or (not (string-prefix-p "class:" scope))
                                    (and (string-prefix-p "class:" scope)
                                         (or (equal scope class-scope) is-member)))
                                (push tag protos)
                                nil))
                          ((cl-struct ctags-xref-tag (kind (or "f" "function")) name scope)
                           (and (or (not (string-prefix-p "class:" scope))
                                    (and (string-prefix-p "class:" scope)
                                         (or (equal scope class-scope) is-member)))
                                (push name fns)
                                t))
                          ((cl-struct ctags-xref-tag (kind (or "x" "externvar")))
                           (push tag externvars)
                           nil)
                          ((cl-struct ctags-xref-tag (kind (or "v" "variable")) name)
                           (push name vars)
                           t)
                          ((cl-struct ctags-xref-tag (kind (or "h" "header" "file")))
                           is-header)
                          (_
                           t))))
                  (ctags-xref--message "filtering item %s...%s"
                                       item (if res "accepted" "ignored"))
                  res))
              items)))
        (dolist (proto protos)
          (let ((name (ctags-xref-location-name (xref-item-location proto))))
            (unless (member name fns)
              (push proto results))))
        (dolist (xvar externvars)
          (let ((name (ctags-xref-location-name (xref-item-location xvar))))
            (unless (member name vars)
              (push xvar results))))
        (ctags-xref--c-sort results)))))

(cl-defmethod xref-backend-definitions :around ((_backend (eql 'ctags)) identifier &context (major-mode c-mode))
  (let ((items (cl-call-next-method)))
    (ctags-xref-c--filter identifier items)))

(provide 'ctags-xref-c)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ctags-xref-c.el ends here
