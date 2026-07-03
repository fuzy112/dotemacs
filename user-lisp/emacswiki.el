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
    "linkd.el"))

;;;###autoload
(defun emacskiwi-update-elisp-files ()
  (interactive)
  (let ((default-directory (file-name-directory (or buffer-file-name load-file-name))))
    (dolist (file emacswiki-elisp-files)
      (let* ((url (format "https://www.emacswiki.org/emacs/download/%s" (url-hexify-string file)))
	     (dest (file-name-nondirectory file)))
	(url-copy-file url dest 'ok-if-already-exists)))))
