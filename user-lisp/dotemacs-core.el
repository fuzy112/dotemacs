;;; dotemacs-core.el --- Core library of DotEmacs -*- lexical-binding: t -*-
;; Copyright © 2024, 2025, 2026  Zhengyi Fu <i@fuzy.me>

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

(defmacro delq! (elt place)
  "Delete members of the list stored in PLACE which are `eq' to ELT.

PLACE is a generalized variable.
The list stored in PLACE is destructively modified.

This is morally equivalent to (setf PLACE (delq ELT PLACE)), except that
PLACE is evaluated only once (after ELT)."
  (macroexp-let2 macroexp-copyable-p x elt
    (gv-letplace (getter setter) place
      (funcall setter `(delq ,x ,getter)))))

(defmacro remq! (elt place)
  "Set PLACE to a copy of PLACE's value with all occurrences of ELT removed.

ELT is compared with elements of PLACE using `eq'.
PLACE is a generalized variable.

This is morally equivalent to (setf PLACE (remq ELT PLACE)), except that
PLACE is evaluated only once (after ELT)."
  (macroexp-let2 macroexp-copyable-p x elt
    (gv-letplace (getter setter) place
      (funcall setter `(remq ,x ,getter)))))

(defmacro delete! (elt place)
  "Delete members of the sequence stored in PLACE which are `equal' to ELT.

PLACE is a generalized variable.  The sequence stored in PLACE may be
destructively modified if the sequence is a list.

This is morally equivalent to (setf PLACE (delete ELT PLACE)), except
that PLACE is evaluated only once (after ELT)."
  (macroexp-let2 macroexp-copyable-p x elt
    (gv-letplace (getter setter) place
      (funcall setter `(delete ,x ,getter)))))

(defmacro remove! (elt place)
  "Set PLACE to a copy of PLACE's value with all occurrences of ELT removed.

ELT is compared with `equal'.
PLACE must be a generalized variable whose value is a sequence.

This is morally equivalent to (setf PLACE (remove ELT PLACE)), except
that PLACE is evaluated only once (after ELT)."
  (macroexp-let2 macroexp-copyable-p x elt
    (gv-letplace (getter setter) place
      (funcall setter `(remove ,x ,getter)))))

(defmacro alist-set! (alist key value &optional testfn)
  "Associate KEY with VALUE in ALIST.
Equality with KEY is tested by TESTFN, defaulting to `eq'."
  `(setf (alist-get ,key ,alist nil nil ,testfn) ,value))

(defmacro alist-del! (alist key &optional testfn)
  "Remove the first element of ALIST whose `car' equals KEY.
Equality with KEY is tested by TESTFN, defaulting to `eq'."
  `(setf (alist-get ,key ,alist :dotemacs-delete t ,testfn) :dotemacs-delete))

(defmacro alist-setq! (alist &rest args)
  "Associate each of KEY with VALUE in ALIST.

If KEY is a symbol, equality is tested by `eq'.
If KEY is an integer, equality is tested by `eql'.
Otherwise, equality is tested by `equal'.

\(fn ALIST &rest [KEY VALUE]...)"
  (declare (indent 1))
  (named-let loop
      ((args args)
       (body nil))
    (if (null args)
        (macroexp-progn body)
      (let ((key (car args))
            (value (cadr args)))
        (when (eq (car-safe key) 'quote)
          (byte-compile-warn-x key "The key should not be quoted"))
        (loop (cddr args)
              (cons `(alist-set! ,alist ,(macroexp-quote key) ,value
                                 ,(macroexp-quote
                                   (cond ((symbolp key) #'eq)
                                         ((integerp key) #'eql)
                                         (t #'equal))))
                    body))))))

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

(defun after-init-1! (func)
  "Execute FUNC after Emacs has finished initialization.
If Emacs has already finished initializing, execute FUNC immediately.
Otherwise, add FUNC to `after-init-hook'."
  (if after-init-time
      (funcall func)
    (add-hook 'after-init-hook func 50)))

(defmacro after-init! (&rest body)
  "Execute BODY after Emacs has finished initialization.
See `after-init-1!'."
  (declare (indent 0))
  (let ((body-var (gensym "body-")))
    `(let ((,body-var (lambda () ,@body)))
       (after-init-1! ,body-var))))

(defmacro with-no-compile! (&rest body)
  "Evaluate BODY without byte-compiling it.
This can be useful for code that should not be byte-compiled.
For example, code that uses macros which might not be
available at compile time."
  (declare (indent 0))
  `(eval ',(macroexp-progn body) lexical-binding))

(defvar dotemacs--project-hooks nil)

(defun dotemacs--project-hook-function (hook &rest args)
  "Call registered functions for HOOK in the current project with ARGS.

See `project-add-hook!'."
  (when-let* ((project (project-current))
              (hooks (alist-get project dotemacs--project-hooks nil t #'equal))
              (functions (alist-get hook hooks)))
    (if (functionp functions)
 (apply functions args)
      (dolist (fun functions)
        (apply fun args)))))

(defun project-add-hook! (hook function &optional depth)
  "Add FUNCTION to HOOK for the current project.
HOOK is the symbol of the hook to which to add.
FUNCTION is the function to add.
Optional DEPTH is the depth at which to add the function (see `add-hook')."
  (let ((project (project-current))
        (project-hook-function (intern (format "dotemacs--project-hook:%S" hook))))
    (defalias project-hook-function (apply-partially #'dotemacs--project-hook-function hook)
      (format "Equivalent to `(apply #\\='dotemacs--project-hook-function \\='%S REST)'.

See `dotemacs--project-hook-function' for details." hook))
    (cl-pushnew function (alist-get hook (alist-get project dotemacs--project-hooks nil t #'equal)))
    (add-hook hook project-hook-function depth)))

(defun project-remove-hook! (hook function &optional project)
  "Remove FUNCTION from HOOK in PROJECT.
PROJECT defaults to the current project."
  (interactive
   (let* ((project (project-current t))
          (hooks (alist-get project dotemacs--project-hooks nil t #'equal))
          (hook (intern (completing-read "Hook: " hooks nil t)))
          (functions (alist-get hook hooks nil t #'equal))
          (function (intern (completing-read "Function: " functions nil t))))
     (list hook function project)))
  (when (null project)
    (setq project (project-current)))
  (let ((functions (alist-get hook (alist-get project dotemacs--project-hooks nil t #'equal))))
    (setf (alist-get hook (alist-get project dotemacs--project-hooks nil t #'equal) nil t)
          (delq function functions))))

(defvar emmip--minor-mode-history nil)
(defvar emmip--major-modes-history nil)

(defun dotemacs--major-mode-completion-table ()
  (let ((modes (seq-filter
                (lambda (x)
                  (and (symbolp x)
                       x
                       (string-suffix-p "-mode" (symbol-name x))))
                (seq-uniq
                 (seq-concatenate
                  'vector
                  (map-values auto-mode-alist)
                  (map-values magic-mode-alist)
                  (map-values interpreter-mode-alist)))))
        (metadata '((category . command))))
    (completion-table-with-metadata modes metadata)))

(defun enable-minor-mode-in-project (mode project major-modes)
  "Enable MODE in all buffers of any MAJOR-MODES in PROJECT."
  (interactive
   (list
    (intern-soft
     (completing-read "Minor mode to enable: "
                      (describe-minor-mode-completion-table-for-symbol)
                      (lambda (mode)
                        (local-variable-if-set-p (intern mode)))
                      (lambda (input)
                        (and-let* ((mode (intern-soft input)))
                          (custom-load-symbol mode)
                          (when-let* ((def (symbol-function mode)))
                            (autoload-do-load def mode))
                          (local-variable-if-set-p mode)))
                      nil
                      'emmip--minor-mode-history))
    (project-current 'maybe-prompt)
    (seq-map #'intern
             (completing-read-multiple
              "Major modes: "
              (dotemacs--major-mode-completion-table)
              nil
              'require-match
              nil
              'emmip--major-modes-history
              (named-let loop
                  ((defaults nil)
                   (mode major-mode))
                (if (null mode)
                    (seq-reverse defaults)
                  (loop (cons (symbol-name mode) defaults)
                        (get mode 'derived-mode-parent))))))))
  (let ((default-directory (project-root project)))
    (custom-load-symbol mode)
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
