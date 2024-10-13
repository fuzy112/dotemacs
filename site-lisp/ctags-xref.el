;;; ctags-xref.el --- XREF backend for Universal-Ctags  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: c, tools
;; Version: 0.5.3

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

;; This package provides an XREF backend for Universal-Ctags,
;; improving code navigation and cross-referencing in Emacs.  It
;; includes functions to convert Universal-Ctags tags into xref items
;; for handling identifiers, definitions, and completion.  The package
;; integrates the XREF backend for ctags with Emacs' xref system and
;; provides support for identifier completion and filtering based on
;; patterns.

;;; Code:

(require 'xref)
(require 'ctags-core)
(eval-when-compile
  (require 'cl-lib))

(defgroup ctags-xref nil
  "Ctags xref."
  :group 'xref
  :group 'ctags
  :prefix "ctags-xref-")

(defface ctags-xref-scope-face
  '((default :inherit completions-annotations))
  "Face for displaying ctags scope.")

(defface ctags-xref-score-face
  '((default :inherit shadow))
  "Face for displaying tag scores.")


;; Define the ctags functions.
(ctags-define-tag ctags-xref-tag
                  input name line pattern content kind scope roles language
                  score)


(cl-defstruct (ctags-xref-location
               (:include xref-file-location)
               (:constructor ctags-xref-make-location (file line column name hint)))
  "Xref location used for ctags."
  name hint)

(cl-defmethod xref-location-marker ((location ctags-xref-location))
  "Return the marker for LOCATION.
The `hint' slot of LOCATION is used to find the position."
  (pcase-let (((cl-struct ctags-xref-location file line column name hint) location))
    (with-current-buffer
        (or (get-file-buffer file)
            (let ((find-file-suppress-same-file-warnings t))
              (find-file-noselect file)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (ignore-errors
            (when (numberp line)
              (forward-line (1- line)))
            (if (or (search-forward-regexp hint nil t)
                    (search-backward-regexp hint nil t))
                (re-search-backward (regexp-opt (list name) 'symbols)
                                    (line-beginning-position)
                                    t)
              (when (numberp column)
                (forward-char column))))
          (point-marker))))))


(defun ctags-xref-make-item (tag)
  "Convert TAG to an xref item.
`ctags-xref-location' is used for the `location' slot."
  (let ((input (ctags-xref-tag-input tag))
        (name (ctags-xref-tag-name tag))
        (line (ctags-xref-tag-line tag))
        (column 0)
        (pattern (ctags-xref-tag-pattern tag))
        (summary (ctags-xref-tag-content tag))
        (scope (ctags-xref-tag-scope tag))
        (score (or (ctags-xref-tag-score tag) 0)))
    (xref-make
     (concat (string-trim (or summary name))
             (and scope
                  (propertize (concat "  " scope)
                              'face 'ctags-xref-scope-face))
             (and score
                  (propertize (concat "\t" (number-to-string score))
                              'face 'ctags-xref-score-face)))
     (ctags-xref-make-location input line column
                               name
                               (or pattern (regexp-quote name))))))

;;;; Xref backend

;;;###autoload
(progn
  (defun ctags-xref-backend ()
    "Return `ctags' if a tags table can be found."
    (when (ctags-tags-file-path)
      (require 'ctags-xref)
      'ctags)))

(defvar ctags-xref-debug nil)

(defun ctags-xref--message (&rest args)
  (if ctags-xref-debug
      (apply #'message args)))

(defvar ctags-xref-filter-functions nil)

(defun ctags-xref-filter (identifier tags)
  (dolist (fn ctags-xref-filter-functions)
    (setq tags (funcall fn identifier tags)))
  tags)

(cl-defmethod xref-backend-definitions ((_backend (eql 'ctags)) identifier)
  (let ((xrefs)
        (tags (ctags-xref-tag-readtags "-PEne" "-" identifier)))
    (setq tags (ctags-xref-filter identifier tags))
    (dolist (tag tags)
      (when-let ((xref (ctags-xref-make-item tag)))
        (push xref xrefs)))
    (nreverse xrefs)))

(declare-function rxt-elisp-to-pcre "pcre2el")

(cl-defmethod xref-backend-apropos ((_backend (eql 'ctags)) pattern)
  (let ((re (xref-apropos-regexp pattern)))
    (setq re (rxt-elisp-to-pcre re))
    (mapcar #'ctags-xref-make-item
            (ctags-xref-tag-readtags
             "-PEneiQ"
             (prin1-to-string
              `((string->regexp ,re :case-fold true)
                $name))
             "-l"))))

(cl-defmethod xref-backend-identifier-completion-table ((_ (eql 'ctags)))
  (let ((buf (current-buffer))
        (input 'init)
        (cands))
    (lambda (str pred action)
      (if (eq action 'metadata)
          '(metadata . ((category . identifier)
                        (annotion-function . ctags-capf--annotate)
                        (company-kind . ctags-capf--company-kind)))
        (when (and (not (string-empty-p str))
                   (or (eq input 'init)
                       (not (string-prefix-p input str))))
          (setq cands (mapcar (lambda (tag)
                                (let ((cand (ctags-xref-tag-name tag)))
                                  (add-text-properties 0 (length cand)
                                                       `(ctags ,tag) cand)
                                  cand))
                              (with-current-buffer buf
                                (ctags-xref-tag-readtags "-PEnep" "-" str)))
                input str))
        (complete-with-action action cands str pred)))))

(provide 'ctags-xref)
;;; ctags-xref.el ends here
