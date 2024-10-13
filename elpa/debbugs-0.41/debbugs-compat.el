;;; debbugs-compat.el --- Compatibility library for debbugs  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hypermedia
;; Package: debbugs

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

;; `main-thread' has been added in Emacs 27.1.
(unless (boundp 'main-thread)
  (defconst main-thread nil))

;; Function `string-replace' is new in Emacs 28.1.
(defalias 'debbugs-compat-string-replace
  (if (fboundp 'string-replace)
      #'string-replace
    (lambda (from-string to-string in-string)
      (let ((case-fold-search nil))
        (replace-regexp-in-string
         (regexp-quote from-string) to-string in-string t t)))))

;; This is needed for Bug#73199.
;; `soap-invoke-internal' let-binds `url-http-attempt-keepalives' to
;; t, which is not thread-safe.  We override this setting.
(defvar url-http-attempt-keepalives)
(defvar debbugs-gnu-use-threads)
(defvar debbugs-compat-url-http-attempt-keepalives nil
  "Temporary storage for `url-http-attempt-keepalives'.")

(defun debbugs-compat-debbugs-advice ()
  "Set `url-http-attempt-keepalives' to nil."
  (setq url-http-attempt-keepalives nil))

(defun debbugs-compat-add-debbugs-advice ()
  "Activate advice for Bug#73199."
  (when debbugs-gnu-use-threads
    (setq debbugs-compat-url-http-attempt-keepalives
          url-http-attempt-keepalives)
    (advice-add
     'url-http-create-request :before #'debbugs-compat-debbugs-advice)))

(defun debbugs-compat-remove-debbugs-advice ()
  "Deactivate advice for Bug#73199."
  (when debbugs-gnu-use-threads
    (setq url-http-attempt-keepalives
          debbugs-compat-url-http-attempt-keepalives)
    (advice-remove 'url-http-create-request  #'debbugs-compat-debbugs-advice)))

(provide 'debbugs-compat)

;;; debbugs-compat.el ends here
