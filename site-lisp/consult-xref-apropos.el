;;; consult-xref-apropos.el --- Consult xref-apropos -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:  Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.2
;; Keywords: programming

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

;; This package `consult-xref-apropos.el' provides a functionality to
;; consult xref-apropos for finding meaningful symbols that match a
;; pattern.  It enhances the xref-apropos functionality by providing
;; dynamic computation of candidates and highlighting matching parts of
;; the candidates.  The package uses Consult framework for interactive
;; selection and completion.
;;
;; The `consult-xref-apropos' function is the main entry point which
;; searches for symbols matching the input pattern.  Users can
;; customize the behavior with various options like prompt, debounce
;; time, and history handling.
;;

;;; Code:

(require 'xref)
(require 'consult-xref)

(defun consult-xref-apropos--highlight-candidate (input)
  "Highlight the candidates for the parts that matches INPUT."
  (let ((words (string-split input "[ \t]")))
    (lambda (cand)
      (let* ((prefix (get-text-property 0 'consult--prefix-group cand))
             (prefix-length (length prefix)))
        (dolist (word words)
          (when (string-match (regexp-quote word) cand prefix-length)
            (add-text-properties (match-beginning 0)
                                 (match-end 0)
                                 '(face consult-highlight-match)
                                 cand))))
      cand)))

(defun consult-xref-apropos--compute (buffer input)
  "Dynamically compute candidates for INPUT in BUFFER."
  (with-current-buffer buffer
    (ignore-errors
      (let* ((consult-xref--fetcher
              (xref--create-fetcher input 'apropos input))
             (candidates (consult-xref--candidates)))
        (mapc (consult-xref-apropos--highlight-candidate input)
              candidates)
        candidates))))

(defun consult-xref-apropos--collection (buffer)
  "Return a completion table for BUFFER."
  (consult--dynamic-collection
   (apply-partially #'consult-xref-apropos--compute buffer)))

;;;###autoload
(defun consult-xref-apropos (initial)
  "Find all meaningful symbols that match the pattern.
INITIAL is the initial input."
  (interactive "P")
  (xref-pop-to-location
   (let ((consult-async-input-debounce .6))
     (consult--read
      (consult-xref-apropos--collection (current-buffer))
      :prompt "Search for pattern: "
      :history 'consult-xref--history
      :require-match t
      :sort nil
      :category 'consult-xref
      :initial (consult--async-split-initial initial)
      :group #'consult--prefix-group
      :state (consult-xref--preview #'switch-to-buffer)
      :lookup (apply-partially #'consult--lookup-prop 'consult-xref)))))

(provide 'consult-xref-apropos)
;;; consult-xref-apropos.el ends here
