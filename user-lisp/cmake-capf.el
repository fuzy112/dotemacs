;;; cmake-capf.el --- Completion-at-point function for CMake  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: convenience
;; Package-Version: 0.1.1

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

;; To use this package, add `cmake-setup-capf' to `cmake-mode-hook' and
;; `cmake-ts-mode-hook'.

;;; Code:

(require 'compat)
(require 'cl-lib)

(defgroup cmake-capf nil
  "Completion-at-point function for CMake."
  :group 'tools
  :group 'programming)

(defcustom cmake-capf-executable "cmake"
  "CMake executable path."
  :type 'string)

(defvar cmake-capf--languages '("CXX" "C" "ASM"))

(defvar cmake-capf--placeholders-alist
  '(("<LANG>" . ("CXX" "C" "ASM"))
    ("<CONFIG>" . ("DEBUG" "RELEASE" "RELWITHDEBINFO" "MINSIZEREL"))
    ("<PROJECT-NAME>" . nil)))

(defvar cmake-capf--candidates nil)

;; TODO tempel support for <PROJECT-NAME>

;; CMake documents use <LANG> as a placeholder for different languages.
;; Expand it to real language name.
(defun cmake-capf--expand (str)
  "Expand placeholders in STR."
  (cl-loop for (k . v) in cmake-capf--placeholders-alist
           if (string-search k str)
           append (mapcar (lambda (i)
                            (propertize (string-replace k i str)
                                        :unexpended str))
                          v)
           into results
           finally return (or results (list str))
           ))

(defun cmake-capf--unexpand (cand)
  "Return the unexpanded expression of CAND.
The returned value can be used to lookup the document."
  (or (get-text-property 0 :unexpanded cand) cand))


(defun cmake-capf--candidates ()
  "Return a list of all candidates."
  (when (null cmake-capf--candidates)
    (dolist (type '(("command" . function)
                    ("module" . module)
                    ("property" . property)
                    ("variable" . variable)
                    ("policy" . magic)))
      (with-temp-buffer
        (call-process cmake-capf-executable nil t nil
                      (format "--help-%s-list" (car type)))
        (dolist (line (string-lines (buffer-string) t))
          (dolist (cand (cmake-capf--expand line))
            (put-text-property 0 (length cand) :company-kind (cdr type) cand)
            (put-text-property 0 (length cand) :type (car type) cand)
            (push cand cmake-capf--candidates))))))
  cmake-capf--candidates)

(defun cmake-capf--company-kind (str)
  "Kind of symbol STR."
  (get-text-property 0 :company-kind str))

(defun cmake-capf--company-doc-buffer (str)
  "Return a documentation buffer for STR."
  (setq str (cmake-capf--unexpand str))
  (let ((bufname (format " *cmake-capf doc for %s*" str))
        (cmd-opt (format "--help-%s" (get-text-property 0 :type str))))
    (unless (get-buffer bufname)
      ;; FIXME cleanup unused buffers
      (with-current-buffer (get-buffer-create bufname)
        (call-process cmake-capf-executable nil t nil cmd-opt str)))
    bufname))

(defun cmake-capf--annotation (cand)
  "Annotate CAND."
  (concat "\t" (capitalize (get-text-property 0 :type cand))))

(defvar cmake-capf--properties
  (list :exclusive 'no
        :company-kind #'cmake-capf--company-kind
        :company-doc-buffer #'cmake-capf--company-doc-buffer
        :annotation-function #'cmake-capf--annotation))

;;;###autoload
(defun cmake-capf (&optional interactive)
  "Complete CMake identifiers.
When called interactively or INTERACTIVE is non-nil, complete the
symbol at point."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions '(cmake-capf)))
        (completion-at-point))
    (pcase (or (bounds-of-thing-at-point 'symbol)
               (cons (point) (point)))
      (`(,beg . ,end)
       `(,beg
         ,end
         ,(cmake-capf--candidates)
         ,@cmake-capf--properties)))))

;;;###autoload
(defun cmake-capf-setup ()
  "Enable `cmake-capf' for the current buffer."
  (add-hook 'completion-at-point-functions #'cmake-capf 0 t))

(provide 'cmake-capf)
;;; cmake-capf.el ends here
