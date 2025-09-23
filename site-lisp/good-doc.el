;;; good-doc.el --- API Documentation Browser -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.1
;; Keywords: doc, help

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is currently a write of my package `gtkdoc.el'.

;;; TODO:

;; Add header
;; Handle out of range exception
;; Fix page navigation

;;; Code:

;;;; Requirements

(require 'browse-url)
(require 'shr)
(require 'dom)
(require 'compat nil t)

;;;; User Options

(defgroup good-doc ()
  "API Documentation Browser."
  :group 'help
  :group 'doc
  :prefix "good-doc-")

(defcustom good-doc-window-select nil
  "Whether to select the Good-Doc window for viewing."
  :type 'boolean)

(defcustom good-doc-cache-timeout 600
  "Timeout for cache."
  :type 'natnum)

;;;; Variables

(defvar-local good-doc-current-docs nil
  "A list of documents used for the current buffer.")

(defvar good-doc-history nil
  "Good-Doc completion history.")

;;;; Variables used during parsing

(defvar good-doc--pages)                ; <sub> tags and the <book> tag
(defvar good-doc--symbols)		; <keyword> tags
(defvar good-doc--entries)		; everything

(defvar good-doc--book-title)		; title attribute of <book>
(defvar good-doc--book-name)		; name attribute of <book>
(defvar good-doc--book-language)	; language attribute of <book>
(defvar good-doc--book-online)		; online attribute of <book>

(defvar good-doc--doc)			; path to the doc

;;;; Parsing functions

(defun good-doc--set-untrusted-content ()
  (set (make-local-variable 'untrusted-contents) t)
  (with-suppressed-warnings ((obsolete untrusted-content))
    (set (make-local-variable 'untrusted-content) t)))

(put 'good-doc--parse 'good-doc-cache-function nil)
(defun good-doc--parse (doc)
  "Parse the DOC index."
  (let ((dom (with-temp-buffer
               (insert-file-contents doc)
               (libxml-parse-xml-region nil nil doc)))
        (good-doc--doc doc)
        good-doc--pages
        good-doc--symbols
        good-doc--entries
        good-doc--book-title
        good-doc--book-name
        good-doc--book-language
        good-doc--book-online)
    (good-doc--parse-1 dom)
    (let ((data
           `((doc . ,doc)
             (title . ,good-doc--book-title)
             (name . ,good-doc--book-name)
             (language . ,good-doc--book-language)
             (online . ,good-doc--book-online)
             (pages . ,(vconcat (nreverse good-doc--pages)))
             (symbols . ,(vconcat (nreverse good-doc--symbols)))
             (entries . ,(vconcat (nreverse good-doc--entries))))))
      (seq-do-indexed (lambda (it it-index)
                        (add-text-properties 0 1 `(index ,it-index) it))
                      (alist-get 'entries data))
      data)))

(defun good-doc--parse-1 (dom)
  (pcase (dom-tag dom)
    ('book
     (setq good-doc--book-title (dom-attr dom 'title)
           good-doc--book-name (dom-attr dom 'name)
           good-doc--book-language (dom-attr dom 'language)
           good-doc--book-online (dom-attr dom 'online))
     (let ((page (substring-no-properties (dom-attr dom 'title))))
       (when (string-empty-p page)
         (setq page (substring-no-properties (dom-attr dom 'name))))
       (when (string-empty-p page)
         (setq page (substring-no-properties (dom-attr dom 'link))))
       (add-text-properties 0 1
                            `( link ,(dom-attr dom 'link)
                               doc ,good-doc--doc)
                            page)
       (cl-pushnew page good-doc--pages :test 'equal)
       (cl-pushnew page good-doc--entries :test 'equal)))
    ('sub
     (let ((page (substring-no-properties (dom-attr dom 'name))))
       (when (string-empty-p page)
         (setq page (substring-no-properties (dom-attr dom 'link))))
       (add-text-properties 0 1
                            `( link ,(dom-attr dom 'link)
                               doc ,good-doc--doc)
                            page)
       (cl-pushnew page good-doc--pages :test 'equal)
       (cl-pushnew page good-doc--entries :test 'equal)))
    ('keyword
     (let ((symbol (substring-no-properties (dom-attr dom 'name))))
       (add-text-properties 0 1
                            `( link ,(dom-attr dom 'link)
                               type ,(dom-attr dom 'type)
                               doc ,good-doc--doc)
                            symbol)
       (cl-pushnew symbol good-doc--symbols :test 'equal)
       (cl-pushnew symbol good-doc--entries :test 'equal))))
  (dolist (it (dom-children dom))
    (good-doc--parse-1 it)))

;;;; Memoization

(defvar good-doc--cache (make-hash-table :test 'equal)
  "Hash table used by the function `good-doc--cache'.")

(defun good-doc--cache (&rest args)
  (if-let* ((data (gethash args good-doc--cache)))
      (prog1 (cdr data)
        (timer-set-time (car data) (time-add nil good-doc-cache-timeout)))
    (let ((val (apply args))
          (timer (run-at-time good-doc-cache-timeout nil
                              #'remhash args good-doc--cache)))
      (prog1 val
        (puthash args (cons timer val) good-doc--cache)))))

(defun good-doc--cache-file (&rest args)
  (let* ((hashcode (sxhash args))
         (file (format "%X" hashcode))
         (default-directory (expand-file-name "good-doc/" user-emacs-directory)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      (let ((val (apply args)))
        (with-temp-file file
          (pp val (current-buffer)))
        val))))

(defun good-doc--memoize-function (symbol)
  (unless (function-get symbol 'good-doc-cache-function)
    (let ((original-function (symbol-function symbol)))
      (fset symbol (apply-partially #'good-doc--cache original-function))
      (function-put symbol 'good-doc-cache-function original-function)
      nil)))

(good-doc--memoize-function #'good-doc--parse)

(defun good-doc--entries (doc)
  (if (consp doc)
      (alist-get 'entries doc)
    (alist-get 'entries (good-doc--parse doc))))

(defun good-doc--pages (doc)
  (if (consp doc)
      (alist-get 'pages doc)
    (alist-get 'pages (good-doc--parse doc))))

(defun good-doc--name (doc)
  (let ((doc (cond ((consp doc) doc)
                   (t (good-doc--parse doc)))))
    (or (alist-get 'title doc)
        (alist-get 'name doc)
        (alist-get 'link doc))))

(defun good-doc--read-doc (prompt docs)
  "Read a document from DOCS.

PROMPT is passed to `completing-read'."
  (let ((cands (mapcar (lambda (doc)
                         (cons (good-doc--name doc) doc))
                       docs)))
    (cdr (assoc (completing-read prompt cands nil t) cands))))

;;;; Documentation Viewer

(defvar good-doc--stack nil
  "List of viewed entries.")

(defvar good-doc--forward-stack nil
  "List of viewed entries for `good-doc-go-forward'.")

(define-derived-mode good-doc-mode special-mode "Good-Doc"
  "Major mode for viewing Devhelp2 documents."
  :interactive nil
  (setq-local browse-url-handlers
              `((good-doc--internal-url-p . good-doc--internal-url-handler)
                ,@browse-url-handlers)
              buffer-undo-list t
              revert-buffer-function #'good-doc--revert-buffer
              truncate-lines t))

;;;; Navigation

(defun good-doc-goto-target ()
  "Go to the original position in a Good-Doc buffer."
  (interactive nil good-doc-mode)
  (goto-char (point-min))
  (when-let* ((entry (car good-doc--stack))
              (id (car (last (string-split (get-text-property 0 'link entry) "#"))))
              (shr-target-id (url-unhex-string id))
              (pred (if (fboundp 'shr--set-target-ids) #'member t))
              (match (text-property-search-forward 'shr-target-id shr-target-id pred)))
    (goto-char (prop-match-beginning match))))

(defun good-doc-go-back ()
  "Go to the previously displayed entry in this DevDocs buffer."
  (interactive nil good-doc-mode)
  (unless (cadr good-doc--stack)
    (user-error "No previous entry"))
  (push (pop good-doc--stack) good-doc--forward-stack)
  (good-doc--render (pop good-doc--stack)))

(defun good-doc-go-forward ()
  "Go to the next entry in this DevDocs buffer."
  (interactive nil good-doc-mode)
  (unless (car good-doc--forward-stack)
    (user-error "No next entry"))
  (good-doc--render (pop good-doc--forward-stack)))

(defun good-doc-next-entry (count)
  "Go forward COUNT entries in this document."
  (interactive "p" good-doc-mode)
  (let* ((current-entry (car good-doc--stack))
         (doc (get-text-property 0 'doc current-entry))
         (index (get-text-property 0 'index current-entry)) ;FIXME
         (next (elt (good-doc--entries doc) (+ index count))))
    (good-doc--render next)))

(defun good-doc-previous-entry (count)
  "Go backward COUNT entries in this document."
  (interactive "p" good-doc-mode)
  (good-doc-next-entry (- count)))

(defun good-doc-goto-page (doc page)
  "Go to a given PAGE of DOC."
  (interactive
   (let* ((current (car good-doc--stack))
          (doc (get-text-property 0 'doc current)))
     (list doc
           (completing-read "Go to page: "
                            (append (good-doc--pages doc) nil)
                            nil t nil 'good-doc-history)))
   good-doc-mode)
  (let* ((page (cond ((stringp page) page)
                     ((numberp page) (elt (good-doc--pages doc) page))))
         (entry (seq-find (lambda (it) (string= it page))
                          (good-doc--entries doc))))
    (good-doc--render entry)))

(defun good-doc-first-page (doc)
  "Go to the first page of DOC."
  (interactive (list (get-text-property 0 'doc (car good-doc--stack)))
               good-doc-mode)
  (good-doc-goto-page doc 0))

(defun good-doc-last-page (doc)
  (interactive (list (get-text-property 0 'doc (car good-doc--stack)))
               good-doc-mode)
  (good-doc-goto-page doc (1- (length (good-doc--pages doc)))))

(defun good-doc-next-page (count)
  "Go forward COUNT pages in this document."
  (interactive "p" good-doc-mode)
  (let* ((current (car good-doc--stack))
         (doc  (get-text-property 0 'doc current))
         (pages (good-doc--pages doc))
         (link (get-text-property 0 'link current))
         (path (car (split-string link "#")))
         (page-title (seq-find (lambda (it) (string= path (get-text-property 0 'link it)))
                               pages))
         (page (seq-position pages page-title)))
    (good-doc-goto-page doc (+ page count))))

(defun good-doc-previous-page (count)
  "Go backward COUNT pages in this document."
  (interactive "p" good-doc-mode)
  (good-doc-next-page (- count)))

(let ((map good-doc-mode-map))
  (define-key map [tab] #'forward-button)
  (define-key map [backtab] #'backward-button)
  (define-key map "p" #'good-doc-previous-entry)
  (define-key map "n" #'good-doc-next-entry)
  (define-key map "g" #'good-doc-goto-page)
  (define-key map "[" #'good-doc-previous-page)
  (define-key map "]" #'good-doc-next-page)
  (define-key map "<" #'good-doc-first-page)
  (define-key map ">" #'good-doc-last-page)
  (define-key map "l" #'good-doc-go-back)
  (define-key map "r" #'good-doc-go-forward)
  (define-key map "." #'good-doc-goto-target)
  (define-key map "i" #'good-doc-lookup))

;;;; Rendering

(defun good-doc--entry-file (entry)
  (let* ((doc (get-text-property 0 'doc entry))
         (link (get-text-property 0 'link entry))
         (path (car (split-string link "#"))))
    (if (string-empty-p path)
        path
      (expand-file-name path (file-name-parent-directory doc)))))

(defun good-doc--filter-doc (doc)
  (when-let* ((body (dom-by-tag doc 'body)))
    (dom-set-attributes body nil))
  (when-let* ((nav (dom-by-class doc "navigation")))
    (dom-remove-node (dom-parent doc nav) nav))
  doc)

(defun good-doc--shr-tag-pre (dom)
  "Render code block in DOM."
  (insert
   (with-temp-buffer
     (shr-tag-pre dom)
     (goto-char (point-min))
     (ignore-errors
       (delay-mode-hooks
         (cond ((looking-at-p "<")
                (nxml-mode))
               ((looking-at-p "\\[")
                (conf-mode))
               ((re-search-forward "from gi\\.repository import" nil t)
                (python-mode))
               ((re-search-forward "imports\\.gi" nil t)
                (javascript-mode))
               (t (c-or-c++-mode)))
         (font-lock-ensure)))
     (buffer-string))))

(defun good-doc--render (entry)
  "Render a Good-Doc ENTRY, returning a buffer."
  (with-current-buffer (get-buffer-create "*good-doc*")
    (good-doc--set-untrusted-content)
    (unless (eq major-mode 'good-doc-mode)
      (good-doc-mode))
    (let ((inhibit-read-only t)
          (shr-external-rendering-functions `((pre . good-doc--shr-tag-pre)
                                              ,@shr-external-rendering-functions))
          (file (good-doc--entry-file entry)))
      (unless (string-empty-p file)
        (erase-buffer)
        (shr-insert-document
         (with-temp-buffer
           (insert-file-contents file)
           (good-doc--filter-doc
            (libxml-parse-html-region))))))
    (set-buffer-modified-p nil)
    (setq-local good-doc-current-docs (list (get-text-property 0 'doc entry)))
    (push entry good-doc--stack)
    (good-doc-goto-target)
    (current-buffer)))

(defun good-doc--revert-buffer (&rest _args)
  "Refresh Good-Doc buffer."
  (good-doc--render (pop good-doc--stack)))

(defun good-doc--internal-url-p (url)
  (not (string-match-p "\\`[a-z]+:" url)))

(defun good-doc--internal-url-handler (url &rest _args)
  (let ((entry (substring-no-properties url)))
    (add-text-properties 0 1 `( link ,url
                                doc ,(get-text-property 0 'doc (car good-doc--stack)))
                         entry)
    (good-doc--render entry)))

;;;; Lookup and Search

(defun good-doc--local-docs ()
  "Find all locally installed devhelp2 files in well-known directories."
  (cl-loop for dir in '("/usr/share/gtk-doc/" "/usr/share/doc/")
           nconc (directory-files-recursively dir "\\.devhelp2\\'" nil nil nil)
           into files
           finally return files))

(defun good-doc--read-docs (prompt &optional multiple)
  "Query interactively for a Good-Doc document.

PROMPT is passed to `completing-read'.
Non-nil MULTIPLE allows selecting multiple documents.
Non-nil AVAILABLE means to offer a list of all available documents;
otherwise, offer only installed documents."
  (let ((cands (mapcar (lambda (it)
                         (let ((name (substring-no-properties (good-doc--name it))))
                           (add-text-properties 0 1 `(doc ,it) name)
                           name))
                       (good-doc--local-docs))))
    (if multiple
        (delq nil (mapcar (lambda (s) (get-text-property 0 'doc (car (member s cands))))
                          (completing-read-multiple prompt cands)))
      (get-text-property 0 'doc
                         (car (member (completing-read prompt cands nil t) cands))))))

(defun good-doc--relevant-docs (ask-docs)
  (if ask-docs
      (let ((docs (good-doc--read-docs "Documents for this buffer: " t)))
        (setq-local good-doc-current-docs docs))
    (or good-doc-current-docs
        (good-doc--relevant-docs t)
        (user-error "No documents"))))

(defun good-doc--annotate (cand)
  (let ((doc (get-text-property 0 'doc cand)))
    (concat " " (propertize " " 'display '(space :align-to 40))
            (good-doc--name doc) "\t"
            (get-text-property 0 'type cand))))

(defun good-doc--entry-candidates (documents)
  (apply #'seq-concatenate 'list (mapcar #'good-doc--entries documents)))

(good-doc--memoize-function #'good-doc--entry-candidates)

(defun good-doc--read-entry (prompt documents initial-input)
  "Read the name of an entry in one of the DOCUMENTS, using PROMPT.

INITIAL-INPUT is passed to `completing-read'."
  (let* ((cands (good-doc--entry-candidates documents))
         (metadata '(metadata
                     (category . good-doc)
                     (annotation-function . good-doc--annotate)))
         (coll (lambda (str pred action)
                 (if (eq action 'metadata)
                     metadata
                   (complete-with-action action cands str pred))))
         (cand (completing-read prompt coll nil t initial-input
                                'good-doc-history
                                (thing-at-point 'symbol))))
    (or (car (member cand cands))
        (user-error "Not an entry!"))))

;;;###autoload
(defun good-doc-lookup (&optional ask-docs initial-input)
  "Look up a Good-Doc documentation entry.

Display entries in the documents `good-doc-current-docs' for
selection.  With a prefix argument (or, from Lisp, if ASK-DOCS is
non-nil), first read the name of one or more installed documents
and set `good-doc-current-docs' for this buffer.

If INITIAL-INPUT is not nil, insert it into the minibuffer."
  (interactive "P")
  (let* ((entry (good-doc--read-entry "Go to documentation: "
                                      (good-doc--relevant-docs ask-docs)
                                      initial-input))
         (buffer (good-doc--render entry))
         (window (display-buffer buffer)))
    (when window
      (with-selected-window window
        (good-doc-goto-target)
        (recenter 0))
      (when good-doc-window-select
        (select-window window)))))

;;;; _

(provide 'good-doc)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (apheleia-mode)
;; eval: (add-hook 'before-save-hook #'whitespace-cleanup nil t)
;; End:

;;; good-doc.el ends here
