;;; site-lisp.el --- Utility for managing site-lisp directory -*- lexical-binding: t -*-
;; Copyright (c) 2024  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "29.4"))
;; Version: 0.1.2
;; Keywords:

;;; Commentary:
;;; Code:

(defvar site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory))

(defvar site-lisp-autoload-file
  (expand-file-name ".autoload.el" site-lisp-directory))

(defvar site-lisp-quickstart-file
  (locate-user-emacs-file "site-lisp-quickstart.el"))

(defun site-lisp-autoload-need-update-p ()
  "Return non-nil if `site-lisp-autoload-file' needs regeneration."
  (or (not (file-exists-p site-lisp-autoload-file))
      (let ((site-lisp-mtime
             (file-attribute-modification-time
	      (file-attributes site-lisp-autoload-file))))
	(seq-find (lambda (file)
		    (let ((mtime (file-attribute-modification-time
				  (file-attributes file))))
		      (time-less-p site-lisp-mtime mtime)))
		  (directory-files-recursively
                   site-lisp-directory
                   "\\.el\\'")))))

(defvar site-lisp-ignore-regexps
  '("/\\.git" "tests?\\'" "CMakeFiles\\'" "scripts?\\'" "data\\'" "build\\'" "assets\\'"
    "images\\'"))

(defun site-lisp--dir-p (file)
  (and (file-directory-p file)
       (not (seq-some (lambda (re) (string-match-p re file)) site-lisp-ignore-regexps))))

(defun site-lisp-dirs ()
  (cons site-lisp-directory
	(seq-filter #'site-lisp--dir-p
		    (directory-files-recursively
		     site-lisp-directory "." t #'site-lisp--dir-p t))))

(defvar site-lisp-dirs nil)

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
    (loaddefs-generate site-lisp-dirs site-lisp-autoload-file)
    (set-file-times site-lisp-autoload-file)
    (byte-recompile-directory site-lisp-directory))
  (load site-lisp-autoload-file nil t))

;;;###autoload
(progn
  (defun site-lisp-activate (&optional arg)
    "Activate site-lisp directories."
    (interactive "P")
    (if (and (file-exists-p site-lisp-quickstart-file)
	     (not arg))
	(load site-lisp-quickstart-file)
      (setq site-lisp-dirs (site-lisp-dirs))
      (setq load-path (append site-lisp-dirs load-path))
      (site-lisp-load-autoloads))))

;;;###autoload
(defun site-lisp-initialize ()
  "Initialize site-lisp."
  (interactive)
  (site-lisp-activate t))

(defun site-lisp-quickstart-refresh ()
  (interactive)
  (let ((coding-system-for-write  'emacs-internal))
    (site-lisp-activate t)
    (delete-file site-lisp-quickstart-file)
    (loaddefs-generate (site-lisp-dirs) site-lisp-autoload-file)
    (with-temp-file site-lisp-quickstart-file
      (pp-emacs-lisp-code
       `(progn
	  (setq site-lisp-dirs ',(site-lisp-dirs))
	  (setq load-path (append site-lisp-dirs load-path))))
      (insert "\n\n")
      (insert-file-contents site-lisp-autoload-file)
      (elisp-enable-lexical-binding))
    (byte-recompile-file site-lisp-quickstart-file t)))

(provide 'site-lisp)
;;; site-lisp.el ends here
