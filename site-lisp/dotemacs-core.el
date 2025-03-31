;;; dotemacs-core.el --- Core library of DotEmacs -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0
;; Keywords:

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

(eval-when-compile (require 'cl-lib))

(defmacro alist-set! (alist key value &optional testfn)
  "Associate KEY with VALUE in ALIST.
Equality with KEY is tested by TESTFN, defaulting to `eq'."
  `(setf (alist-get ,key ,alist nil nil ,testfn) ,value))

(defmacro alist-del! (alist key &optional testfn)
  "Remove the first element of ALIST whose `car' equals KEY.
Equality with KEY is tested by TESTFN, defaulting to `eq'."
  `(setf (alist-get ,key ,alist nil t ,testfn) nil))

(defmacro alist-setq! (alist &rest args)
  "Associate each of KEY with VALUE in ALIST.
If KEY is a symbol, equality is tested by `eq'.
If KEY is an integer, equality is tested by `eql'.
Otherwise, equality is tested by `equal'.

\(fn ALIST &rest [KEY VALUE]...)"
  (declare (indent 1))
  (cl-loop for (key value) on args by #'cddr
           when (eq (car-safe key) 'quote)
           do (byte-compile-warn-x key "The key should not be quoted")
           collect `(alist-set! ,alist ,(macroexp-quote key) ,value
                                ,(macroexp-quote
                                  (cond ((symbolp key) #'eq)
                                        ((integerp key) #'eql)
                                        (t #'equal))))
           into body
           finally return (macroexp-progn body)))

(defmacro alist-delq! (alist &rest keys)
  "Remove elements of ALIST whose `car' equals to any of KEYS.
If the key is a symbol, equality is tested by `eq'.
If the key is an integer, equality is tested by `eql'.
Otherwise, equality is tested by `equal'."
  (declare (indent 1))
  (macroexp-progn
   (mapcar (lambda (key)
             (when (eq (car-safe key) 'quote)
               (byte-compile-warn-x key "The key should not be quoted"))
             `(alist-del! ,alist ,(macroexp-quote key)
                          ,(macroexp-quote
                            (cond ((symbolp key) #'eq)
                                  ((integerp key) #'eql)
                                  (t #'equal)))))
           keys)))

(defmacro after-load-1! (spec &rest body)
  "Evaluate BODY after the specified features or files are loaded.
SPEC could be
- a symbol
- a string
- (SPECS...)
- (:and SPECS...)
- (:or SPEC...)."
  (declare (indent 1))
  (unless lexical-binding
    (error "after-load! requires lexical-binding to be t"))
  (pcase-exhaustive spec
    ('nil
     (macroexp-progn body))
    (`(quote ,unquoted)
     `(after-load-1! ,unquoted ,@body))
    ((or (pred stringp) (pred symbolp))
     `(with-eval-after-load ,(macroexp-quote spec)
        ,@body))
    (`(:or . ,sub-specs)
     (let ((lambda-var (gensym "body-")))
       `(let ((,lambda-var (lambda () ,@body)))
          ,(macroexp-progn
            (mapcar (lambda (sub-spec)
                      `(after-load-1! ,sub-spec
                         (funcall (prog1
                                      ,lambda-var
                                    (setq ,lambda-var #'ignore)))))
                    sub-specs)))))
    ((or `(:and . ,sub-specs)
         (and (pred listp) sub-specs))
     `(after-load-1! ,(car sub-specs)
        (after-load-1! ,(cdr sub-specs) ,@body)))))

(defmacro after-load! (spec &rest body)
  "Similar to `after-load-1!', but suppress warnings in BODY.
See `after-load-1!' for SPEC."
  (declare (indent 1))
  `(after-load-1! ,spec (with-no-warnings ,@body)))

(defvar dotemacs--project-hooks nil)

(defun dotemacs--project-hook-function (hook &rest args)
  (when-let* ((project (project-current))
              (hooks (alist-get project dotemacs--project-hooks nil t #'equal))
              (functions (alist-get hook hooks)))
    (if (functionp functions)
        (funcall functions)
      (mapcar #'funcall functions))))

(defun project-add-hook! (hook function &optional depth)
  (let ((project (project-current))
        (project-hook-function (intern (format "dotemacs--project-hook:%S" hook))))
    (defalias project-hook-function (apply-partially #'dotemacs--project-hook-function hook))
    (cl-pushnew function (alist-get hook (alist-get project dotemacs--project-hooks nil t #'equal)))
    (add-hook hook project-hook-function depth)))

(defvar emmip--minor-mode-history nil)
(defvar emmip--major-modes-history nil)

(defun dotemacs--major-mode-completion-table ()
  (let ((modes (seq-filter
                (lambda (x)
                  (and (symbolp x)
                       x
                       (string-suffix-p "-mode" (symbol-name x))))
                (map-values auto-mode-alist))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          '(metadata . ((category . command)))
        (complete-with-action action modes str pred)))))

(defun enable-minor-mode-in-project (mode project major-modes)
  "Enable MODE in all buffers of any MAJOR-MODES in PROJECT."
  (interactive
   (list
    (intern-soft
     (completing-read "Minor mode to enable: "
                      (describe-minor-mode-completion-table-for-symbol)
                      (lambda (mode)
                        (local-variable-if-set-p (intern mode)))
                      'require-match
                      nil
                      'emmip--minor-mode-history))
    (project-current 'maybe-prompt)
    (mapcar #'intern-soft
            (completing-read-multiple
             "Major modes: "
             (dotemacs--major-mode-completion-table)
             nil
             'require-match
             nil
             'emmip--major-modes-history
             (let ((values nil)
                   (mode major-mode))
               (while mode
                 (push (symbol-name mode) values)
                 (setq mode (get mode 'derived-mode-parent)))
               (nreverse values))))))
  (let ((default-directory (project-root project)))
    (dolist (mm major-modes)
      (custom-load-symbol mm)
      (let ((hook (intern-soft (format "%S-hook" mm))))
        (unless hook
          (error "Major mode hook `%S' is not defined" hook))
        (project-add-hook! hook mode))))
  (dolist (buf (project-buffers project))
    (with-current-buffer buf
      (when (derived-mode-p major-modes)
        (funcall mode)))))

(provide 'dotemacs-core)
;;; dotemacs-core.el ends here
