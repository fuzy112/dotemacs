;;; site-lisp.el --- Utility for managing site-lisp directory -*- lexical-binding: t -*-
;; Copyright (c) 2024  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "29.4"))
;; Version: 0.1.0
;; Keywords:

;;; Commentary:
;;; Code:

(defvar site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory))

(defvar site-lisp-autoload-file
  (expand-file-name ".autoload.el" site-lisp-directory))

(defun site-lisp-autoload-need-update-p ()
  "Return non-nil if `site-lisp-autoload-file' needs regeneration."
  (or (not (file-exists-p site-lisp-autoload-file))
      (let ((site-lisp-mtime
             (file-attribute-modification-time
              (file-attributes site-lisp-autoload-file))))
        (catch 'found
          (dolist (file (directory-files-recursively
                         site-lisp-directory
                         "\\.el\\'"))
            (let ((mtime (file-attribute-modification-time
                          (file-attributes file))))
              (when (time-less-p site-lisp-mtime mtime)
                (throw 'found t))))
          nil))))

(defun site-lisp-dirs ()
  (cons site-lisp-directory
        (seq-filter #'file-directory-p
                    (directory-files-recursively
                     site-lisp-directory "." t))))

(defvar site-lisp-dirs (site-lisp-dirs))

(defun site-lisp-byte-compile-all ()
  "Byte compile all Emacs lisp files in `site-lisp-directory'."
  (interactive)
  (dolist (file (directory-files site-lisp-directory 'full
                                 "\\.el\\'" t))
    (byte-compile-file file)))

(defun site-lisp-load-autoloads ()
  "Load autoloads for site-lisp files."
  (interactive)
  (when (site-lisp-autoload-need-update-p)
    (loaddefs-generate (site-lisp-dirs) site-lisp-autoload-file)
    (set-file-times site-lisp-autoload-file)
    (byte-recompile-directory site-lisp-directory))
  (load site-lisp-autoload-file nil t))

(defun site-lisp-activate ()
  "Activate site-lisp directories."
  (interactive)
  (setq load-path (append site-lisp-dirs load-path))
  (site-lisp-load-autoloads))

(provide 'site-lisp)
;;; site-lisp.el ends here
