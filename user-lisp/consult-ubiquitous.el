;;; consult-ubiquitous.el --- Extra integration for Consult -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "32.0.50"))
;; Version: 0.1.0
;; Keywords: matching, files, completion

;;; Commentary:
;;
;; This package provides Consult-based replacements for several standard
;; Emacs completion readers.  Enabling `consult-ubiquitous-mode' installs
;; these replacements globally, so commands and packages that use the
;; standard reading APIs benefit from Consult-style preview, narrowing,
;; and sorting.
;;
;; To use this package, simply enable the global minor mode:
;;
;;     (consult-ubiquitous-mode 1)
;;
;;; Code:

(require 'consult)

(defgroup consult-ubiquitous nil
  "Consult-based replacements for standard completion functions."
  :group 'consult)

(declare-function project--completing-read-strict "project")
(declare-function project-prompt-project-dir "project")
(declare-function project-root "project")
(declare-function project--delete-zombie-projects "project")
(declare-function project--ensure-read-project-list "project")

(defun consult-ubiquitous-read-file-name (prompt &optional dir default mustmatch initial pred)
  "Consult-based replacement for `read-file-name'.
PROMPT, DIR, DEFAULT, MUSTMATCH, INITIAL, and PRED are as for
`read-file-name'.  This function uses `consult--read-1' with file
preview and `substitute-in-file-name' lookup."
  (let* ((default-directory (abbreviate-file-name (or dir default-directory "~/")))
         (minibuffer-completing-file-name t)
         (pred (or pred 'file-exists-p))
         (completion-ignore-case read-file-name-completion-ignore-case))
    (consult--read-1
     #'read-file-name-internal
     :state (consult--file-preview)
     :prompt prompt
     :initial (if initial
                  (expand-file-name initial)
                default-directory)
     :require-match mustmatch
     :history 'file-name-history
     :default (or default "")
     :predicate pred
     :preview-key consult-preview-key
     :sort t
     :lookup (lambda (selected &rest _) (substitute-in-file-name selected)))))

(defun consult-ubiquitous-read-buffer (prompt &optional def mustmatch pred)
  "Consult-based replacement for `read-buffer'.
PROMPT, DEF, MUSTMATCH, and PRED are as for `read-buffer'.  This
function uses `consult--read-1' with buffer preview."
  (let ((completion-ignore-case read-buffer-completion-ignore-case)
        (default (if (bufferp def) (buffer-name def) def)))
    (consult--read-1
     #'internal-complete-buffer
     :state         (consult--buffer-preview)
     :default       (or default "")
     :prompt        (format-prompt (replace-regexp-in-string ":[[:space:]]*\\'" "" prompt) def)
     :require-match mustmatch
     :history       'buffer-name-history
     :predicate     pred
     :preview-key   consult-preview-key
     :sort          t
     :lookup        (lambda (selected &rest _) selected))))

(defun consult-ubiquitous-completing-read-strict
    (prompt collection &optional predicate hist mb-default common-parent-directory)
  "Consult-based replacement for `project--completing-read-strict'.
PROMPT, COLLECTION, PREDICATE, HIST, MB-DEFAULT, and
COMMON-PARENT-DIRECTORY are as for `project--completing-read-strict'.
This function uses `consult--read-1' with a file preview and requires
matching input to be confirmed."
  (cl-letf* ((mb-default (mapcar (lambda (mb-default)
                                   (if (and common-parent-directory
                                            mb-default
                                            (file-name-absolute-p mb-default))
                                       (file-relative-name
                                        mb-default common-parent-directory)
                                     mb-default))
                                 (if (listp mb-default) mb-default (list mb-default))))
             (abs-cpd (expand-file-name (or common-parent-directory "")))
             (abs-cpd-length (length abs-cpd))
             (non-essential t)          ;Avoid new Tramp connections.
             ((symbol-value hist)
              (if common-parent-directory
                  (mapcan
                   (lambda (s)
                     (setq s (expand-file-name s))
                     (and (string-prefix-p abs-cpd s)
                          (not (eq abs-cpd-length (length s)))
                          (list (substring s abs-cpd-length))))
                   (symbol-value hist))
                (symbol-value hist))))
    (consult--read-1 collection
                     :prompt (format "%s: " prompt)
                     :predicate predicate
                     :require-match 'confirm
                     :history hist
                     :default nil
                     :add-history mb-default
                     :sort t
                     :lookup (lambda (selected &rest _) selected)
                     :state (consult--file-preview)
                     :preview-key consult-preview-key)))

;;;###autoload
(define-minor-mode consult-ubiquitous-mode
  "Global minor mode that uses Consult for standard completion readers.
When enabled, this mode advises standard Emacs completion readers to
use Consult-based equivalents.  See the `consult-ubiquitous-*'
functions for details."
  :global t
  :lighter " Consult-Ubiq"
  (advice-remove #'read-file-name #'consult-ubiquitous-read-file-name)
  (advice-remove #'read-buffer #'consult-ubiquitous-read-buffer)
  (advice-remove #'project--completing-read-strict #'consult-ubiquitous-completing-read-strict)
  (when consult-ubiquitous-mode
    (advice-add #'read-file-name :override #'consult-ubiquitous-read-file-name)
    (advice-add #'read-buffer :override #'consult-ubiquitous-read-buffer)
    (advice-add #'project--completing-read-strict :override #'consult-ubiquitous-completing-read-strict)))

(provide 'consult-ubiquitous)
;;; consult-ubiquitous.el ends here
