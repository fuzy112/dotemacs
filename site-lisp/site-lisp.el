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

(defvar site-lisp-skip-check nil)

(defun site-lisp-autoload-need-update-p ()
  "Return non-nil if `site-lisp-autoload-file' needs regeneration."
  (or (not (file-exists-p site-lisp-autoload-file))
      (if site-lisp-skip-check
	  nil
	(let ((site-lisp-mtime
               (file-attribute-modification-time
		(file-attributes site-lisp-autoload-file))))
	  (seq-find (lambda (file)
		      (let ((mtime (file-attribute-modification-time
				    (file-attributes file))))
			(time-less-p site-lisp-mtime mtime)))
		    (directory-files-recursively
                     site-lisp-directory
                     "\\.el\\'"))))))

(defun site-lisp--dir-p (file)
  (let ((basename (file-name-nondirectory file)))
    (and (file-directory-p file)
	 (not (string-prefix-p "." basename))
	 (not (string-match-p "tests?\\'" file))
	 (not (string-match-p "CMakeFiles\\'" file))
	 (not (string-match-p "scripts?\\'" file))
	 (not (string-match-p "data\\'" file))
	 (not (string-match-p "build\\'" file))
	 (not (string-match-p "assets\\'" file)))))

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
(defun site-lisp-activate ()
  "Activate site-lisp directories."
  (interactive)
  (setq site-lisp-dirs (site-lisp-dirs))
  (setq load-path (append site-lisp-dirs load-path))
  (site-lisp-load-autoloads))

(provide 'site-lisp)
;;; site-lisp.el ends here
