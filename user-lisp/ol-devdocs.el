;;; ol-devdocs.el --- Org-link for Devdocs -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "29.1") (devdocs "0.7"))
;; Version: 0.1.0
;; Keywords: hypermedia

;;; Commentary:
;;; Code:

(require 'ol)
(eval-when-compile (require 'let-alist))

(defvar devdocs--stack)
(declare-function devdocs-goto-page "ext:devdocs.el")

;;;###autoload
(defun ol-devdocs-open (path _)
  (require 'devdocs)
  (string-match "\\(.*?\\)\\(?:/\\(.*\\)\\)?$" path)
  (let* ((slug (match-string 1 path))
	 (doc (cl-find slug (devdocs--installed-docs)
		       :test #'string=
		       :key (apply-partially #'alist-get 'slug)))
	 (doc-path (match-string 2 path)))
    (pop-to-buffer
     (devdocs-goto-page doc doc-path))))

;;;###autoload
(defun ol-devdocs-export (link description backend)
  (let ((path (format "https://devdocs.io/%s" link))
	(desc (or description link)))
    (cond
     ((eq backend 'html) (format "<a target=\"_blank\" rel=\"noopener noreferrer\" href=\"%s\">%s</a>" path desc))
     ((eq backend 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq backend 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq backend 'ascii) (format "[%s] (<%s>)" desc path))
     ((eq backend 'md) (format "[%s](%s)" desc path))
     (t path))))

;;;###autoload
(defun ol-devdocs-store-link (&optional _interactive?)
  (when (derived-mode-p 'devdocs-mode)
    (let-alist (car devdocs--stack)
      (let* ((slug (alist-get 'slug .doc))
	     (path .path)
	     (link (concat "devdocs:" slug "/" path))
	     (description .name))
	(org-link-store-props
	 :type "devdocs"
	 :link link
	 :description description)))))

;;;###autoload
(with-eval-after-load 'ol
  (org-link-set-parameters "devdocs"
			   :follow #'ol-devdocs-open
			   :export #'ol-devdocs-export
			   :store #'ol-devdocs-store-link))

(provide 'ol-devdocs)
;;; ol-devdocs.el ends here
