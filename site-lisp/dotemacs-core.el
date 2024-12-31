;;; dotemacs-core.el --- Core library of DotEmacs -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "30.0.92"))
;; Version: 0.1.0
;; Keywords:

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

(provide 'dotemacs-core)
;;; dotemacs-core.el ends here
