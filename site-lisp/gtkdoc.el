;;; gtkdoc.el --- Look up symbols in devhelp -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.4
;; Keywords: docs, help

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provide a command to look up symbols in devhelp
;; documents.  Devhelp is the document format used by the Gtk project.
;;
;; By default, this package uses a predefined list of online document
;; sources.  If you prefer, you can customize `gtkdoc-doc-list' to
;; specify a list of devhelp2 files on you local machine.
;; Specifically, the function `gtkdoc-local-docs' can be used to find
;; all devhelp2 files in well known directories.
;;

;;; Code:

(require 'url-expand)
(require 'url-handlers)
(require 'dom)
(require 'shr)
(require 'compat nil t)

(warn "This package is deprecated.  Do not use!!!")

(defgroup gtkdoc ()
  "Facility for looking up Gtk documentation."
  :group 'docs
  :group 'c
  :prefix "gtkdoc-")

(defcustom gtkdoc-cache-directory (expand-file-name "gtkdoc" user-emacs-directory)
  "The directory where cached gtkdoc indices are stored."
  :type 'path)

(defcustom gtkdoc-browse-url-function browse-url-browser-function
  "The function used to browse the html document."
  :type 'function)

(defvar gtkdoc-lookup-history nil)

;;;###autoload
(defun gtkdoc-local-docs ()
  "Find all devhelp2 files in well known directories."
  (cl-loop for dir in '("/usr/share/gtk-doc/" "/usr/share/doc/")
           nconc (directory-files-recursively dir "\\.devhelp2\\'" nil nil t)
           into files
           finally return (mapcar (apply-partially #'concat "file://") files)))

(defcustom gtkdoc-doc-list
  '("https://docs.gtk.org/Pango/Pango.devhelp2"
    "https://docs.gtk.org/atk/atk.devhelp2"
    "https://docs.gtk.org/gdk3/gdk3.devhelp2"
    "https://docs.gtk.org/gio/gio-2.0.devhelp2"
    "https://docs.gtk.org/glib/glib-2.0.devhelp2"
    "https://docs.gtk.org/gmodule/gmodule-2.0.devhelp2"
    "https://docs.gtk.org/gobject/gobject-2.0.devhelp2"
    "https://docs.gtk.org/gtk3/gtk3.devhelp2"
    "https://gnome.pages.gitlab.gnome.org/libadwaita/doc/1-latest/libadwaita-1.devhelp2"
    "https://gnome.pages.gitlab.gnome.org/libnma/libnma.devhelp2"
    "https://libproxy.github.io/libproxy/libproxy-1.0.devhelp2"
    "https://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-6.0.devhelp2"
    "https://www.cairographics.org/manual/cairo.devhelp2"
    "https://www.networkmanager.dev/docs/libnm/latest/libnm.devhelp2")
  "A list of urls to devhelp2 document indices."
  :type '(repeat string))

(defun gtkdoc-clear-cache ()
  "Clear cached gtk doc indices."
  (interactive)
  (delete-directory gtkdoc-cache-directory t))

(defmacro gtkdoc--with-url-file-handler (&rest body)
  (declare (indent 0) (debug fbody))
  `(cl-letf (((alist-get url-handler-regexp file-name-handler-alist)
              #'url-file-handler))
     ,@body))

(cl-defmethod gtkdoc--cache-file-name (url)
  (file-name-with-extension
   (expand-file-name (url-hexify-string url) gtkdoc-cache-directory)
   ".eld"))

(cl-defmethod gtkdoc--parse-xml (url)
  (gtkdoc--with-url-file-handler
   (with-work-buffer
     (insert-file-contents url)
     (libxml-parse-xml-region  nil nil url))))

(cl-defmethod gtkdoc--extract-symbols (url)
  (unless (and (file-exists-p gtkdoc-cache-directory)
               (file-directory-p gtkdoc-cache-directory))
    (mkdir gtkdoc-cache-directory))
  (let ((cache-file-name (gtkdoc--cache-file-name url)))
    (if (file-exists-p cache-file-name)
        (with-work-buffer
          (insert-file-contents cache-file-name)
          (goto-char (point-min))
          (read (current-buffer)))
      (let ((result (gtkdoc--extract-symbols-no-cache url))
            pp-max-width)
        (with-temp-file cache-file-name
          (add-file-local-variable-prop-line 'mode 'lisp-data)
          (add-file-local-variable-prop-line 'coding buffer-file-coding-system)
          (goto-char (point-max))
          (newline)
          (pp result (current-buffer)))
        result))))

(defvar gtkdoc--symbols)

(cl-defmethod gtkdoc--extract-symbols-no-cache (url)
  (let ((xml (gtkdoc--parse-xml url))
        (gtkdoc--symbols nil))
    (gtkdoc--extract-symbols-no-cache-1 xml (file-name-directory url))
    (nreverse gtkdoc--symbols)))

(defvar gtkdoc--book nil)

(cl-defmethod gtkdoc--extract-symbols-no-cache-1 (xml prefix)
  (pcase (dom-tag xml)
    ((or 'sub 'keyword)
     (let* ((name (dom-attr xml 'name))
            (type (dom-attr xml 'type))
            (link (dom-attr xml 'link))
            (full-link (propertize (url-expand-file-name link prefix)))
            (prefix (and (string-match "\\`\\(.*\\)\\." name)
                         (match-string 1 name))))
       (add-text-properties 0 (length name) `( gtkdoc-link ,full-link
                                               gtkdoc-type ,type
                                               gtkdoc-prefix ,prefix
                                               gtkdoc-book ,gtkdoc--book)
                            name)
       (push name gtkdoc--symbols)))
    ('book
     (let ((gtkdoc--book (dom-attr xml 'title)))
       (dolist (child (dom-children xml))
         (gtkdoc--extract-symbols-no-cache-1 child prefix))))
    (_ (dolist (child (dom-children xml))
         (gtkdoc--extract-symbols-no-cache-1 child prefix)))))

(cl-defmethod gtkdoc--annotate (str)
  (concat
   " "
   (propertize " " 'display '(space :align-to 40))
   (get-text-property 0 'gtkdoc-type str)
   (propertize " " 'display '(space :align-to 60))
   "*"
   (get-text-property 0 'gtkdoc-book str)
   "*"))

(cl-defmethod gtkdoc--group (completion transform)
  (let ((prefix (get-text-property 0 'gtkdoc-prefix completion)))
    (if transform
        (if (stringp prefix)
            (substring completion (1+ (length prefix)))
          completion)
      (or prefix "<global>"))))

(cl-defmethod gtkdoc--default-candidate (candidates)
  (and-let* ((symbol (thing-at-point 'symbol))
             (regexp (format "\\_<%s\\_>" (regexp-quote symbol))))
    (cl-find-if (apply-partially #'string-match-p regexp)
                candidates)))

(cl-defmethod gtkdoc--browse-url (url)
  (let ((browse-url-browser-function gtkdoc-browse-url-function))
    (browse-url url)))

(cl-defmethod gtkdoc--internal-url-p (url)
  (let ((url (url-expand-file-name url)))
    (cl-some (lambda (devhelp)
               (string-prefix-p (url-file-directory devhelp) url))
             gtkdoc-doc-list)))

(define-derived-mode gtkdoc-mode special-mode "GtkDoc"
  "Major mode for displaying Gtk docs."
  (setq-local browse-url-handlers `((gtkdoc--internal-url-p . gtkdoc--url-handler)
                                    ,@browse-url-handlers)
              buffer-undo-list nil
              truncate-lines t))

(defcustom gtkdoc-fontify-code-blocks t
  "If non-nil, fontify code blocks in the documentation."
  :type 'boolean)

(cl-defmethod gtkdoc--shr-tag-pre (dom)
  "Fontify DOM as code block."
  (let ((start (point)))
    (if-let* ((mode (and gtkdoc-fontify-code-blocks
			 (if (fboundp 'c-ts-mode) 'c-ts-mode
                           'c-mode))))
	(insert
	 (with-work-buffer
	   (shr-tag-pre dom)
	   (let ((inhibit-message t)
		 (message-log-max nil))
	     (ignore-errors (delay-mode-hooks (funcall mode)))
	     (font-lock-ensure))
	   (buffer-string)))
      (shr-tag-pre dom))
    (add-face-text-property start (point) 'gtkdoc-code-block t)))

(cl-defmethod gtkdoc--render (url)
  (with-current-buffer (get-buffer-create "*gtkdoc*")
    (unless (derived-mode-p 'gtkdoc-mode)
      (gtkdoc-mode))
    (let ((inhibit-read-only t)
	  (shr-external-rendering-functions `((pre . gtkdoc--shr-tag-pre)
					      ,@shr-external-rendering-functions)))
      (erase-buffer)
      (shr-insert-document
       (with-work-buffer
	 (url-insert-file-contents url)
	 (libxml-parse-html-region nil nil)))
      (set-buffer-modified-p nil))
    (setq url-current-object (url-generic-parse-url url))
    (let ((shr-target-id (url-target url-current-object)))
      (goto-char (point-min))
      (when-let* ((match (text-property-search-forward 'shr-target-id shr-target-id #'member)))
        (goto-char (prop-match-beginning match))))
    (current-buffer)))

(cl-defmethod gtkdoc-goto-page (link)
  (with-current-buffer (gtkdoc--render link)
    (pop-to-buffer (current-buffer))))

(cl-defmethod gtkdoc--url-handler (url &rest _args)
  (gtkdoc-goto-page (url-expand-file-name url)))

;;;###autoload
(defun gtkdoc-lookup ()
  "Look up a symbol in Gtk documentation."
  (interactive)
  (let* ((cands (mapcan #'gtkdoc--extract-symbols gtkdoc-doc-list))
         (default (gtkdoc--default-candidate cands))
         (metadata `(metadata . ((category . gtkdoc-entry)
                                 (annotation-function . ,#'gtkdoc--annotate)
                                 (group-function . ,#'gtkdoc--group))))
         (collection (lambda (str pred action)
                       (if (eq action 'metadata)
                           metadata
                         (complete-with-action action cands str pred))))
         (selected (completing-read (format-prompt "Go to documentation" default)
                                    collection nil t nil nil default))
         (cand (car (or (member selected cands)
                        (user-error "Invalid candidate"))))
         (link (get-text-property 0 'gtkdoc-link cand)))
    (gtkdoc-goto-page link)))

(provide 'gtkdoc)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (apheleia-mode)
;; End:

;;; gtkdoc.el ends here
