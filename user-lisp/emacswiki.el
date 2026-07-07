#!/usr/bin/env -S emacs -x
;;   -*- lexical-binding: t; -*-

(defvar emacswiki-elisp-files
  '(
    ;; bookmark+
    "bookmark+-1.el"
    "bookmark+-bmu.el"
    "bookmark+-chg.el"
    "bookmark+-doc.el"
    "bookmark+-key.el"
    "bookmark+-lit.el"
    "bookmark+-mac.el"
    "bookmark+.el"

    ;; dired+
    "dired+.el"

    ;; linkd
    "linkd.el"

    ;; help+
    "help+.el"

    ;; help-fns+
    "help-fns+.el"

    ;; info+
    "info+.el"

    ;; thingatpt+
    "thingatpt+.el"))

;;;###autoload
(defun emacswiki-update-all-elisp-files ()
  "Update all EmacsWiki ELisp files listed in `emacswiki-elisp-files'."
  (interactive)
  (dolist (file emacswiki-elisp-files)
    (emacswiki-update-elisp-file file)))

;;;###autoload
(defun emacswiki-update-elisp-file (file)
  "Download and replace FILE from EmacsWiki.
FILE should be a filename (without directory) that exists on EmacsWiki."
  (interactive (list (completing-read "File: " emacswiki-elisp-files)))
  (let ((default-directory user-lisp-directory))
    (let* ((url (format "https://www.emacswiki.org/emacs/download/%s" (url-hexify-string file)))
	   (dest (file-name-nondirectory file)))
      (url-copy-file url dest 'ok-if-already-exists))))

(provide 'emacswiki)
;;; emacswiki.el ends here
