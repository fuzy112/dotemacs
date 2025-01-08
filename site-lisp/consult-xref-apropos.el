;;; consult-xref-apropos.el --- Consult xref-apropos -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:  Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.2
;; Keywords: programming
;; Package-Requires: ((consult "1.10"))

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

(defun consult-xref-apropos--compute (buffer input &optional callback)
  "Dynamically compute candidates for INPUT in BUFFER."
  (with-current-buffer buffer
    (ignore-errors
      (let* ((consult-xref--fetcher
              (xref--create-fetcher input 'apropos input))
             (candidates (consult-xref--candidates)))
        (mapc (consult-xref-apropos--highlight-candidate input)
              candidates)
        (when callback
          (funcall callback candidates))
        candidates))))

;;;###autoload
(defun consult-xref-apropos (initial)
  "Find all meaningful symbols that match the pattern.
INITIAL is the initial input."
  (interactive "P")
  (xref-pop-to-location
   (consult--read
    (consult--dynamic-collection
        (apply-partially #'consult-xref-apropos--compute (current-buffer))
      :highlight (lambda (input)
                   (apply-partially
                    #'consult--highlight-regexps
                    (mapcar #'regexp-quote (split-string input)) t))
      :min-input 3
      :debounce .5
      :throttle .5)
    :prompt "Search for pattern: "
    :history 'consult-xref--history
    :require-match t
    :sort nil
    :category 'consult-xref
    :initial initial
    :group #'consult--prefix-group
    :state (consult-xref--preview #'switch-to-buffer)
    :lookup (apply-partially #'consult--lookup-prop 'consult-xref))))

(provide 'consult-xref-apropos)
;;; consult-xref-apropos.el ends here
