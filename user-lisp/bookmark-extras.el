;;; bookmark-extras.el --- Bookmark support for various modes -*- lexical-binding: t -*-
;; Copyright © 2024, 2025, 2026  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.2
;; Keywords: convenience

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

;; This Emacs package, `bookmark-extras.el', provides bookmark
;; support for various modes and defines new bookmark types.  It
;; includes functions for creating bookmark records and handlers for
;; jumping to bookmarked entries.

;; Supported modes:
;;  - mu4e-main-mode
;;  - compilation-mode (including compilation-minor-mode and
;;    compilation-shell-minor-mode)
;;  - eat-mode
;;  - deadgrep-mode
;;  - telega-root-mode and telega-chat-mode

;; Additionally, it provides a browser‑independent URL bookmark type
;; (`url-bookmark-jump' / `url-bookmark-add') and an Org link bookmark
;; type (`org-link-bookmark-jump' / `org-link-bookmark-set').

;;; Code:

(require 'bookmark)

(eval-when-compile
  (require 'let-alist))

(defgroup bookmark-extras ()
  "Enhancements to bookmark functionality."
  :group 'bookmark)

;;;; Utility

(defun bookmark-display-buffer (buffer)
  (if (bound-and-true-p bmkp-jump-display-function)
      (funcall bmkp-jump-display-function buffer)
    (pop-to-buffer-same-window buffer))
  (set-buffer buffer))

(defun bookmark-completing-read* (handler prompt &optional default)
  (bookmark-maybe-load-default-file)
  (let ((bookmark-alist
         (map-filter (lambda (_ record)
                       (eq (alist-get 'handler record) handler))
                     bookmark-alist)))
    (unless default
      (setq default (caar bookmark-alist)))
    (bookmark-completing-read prompt default)))

;;;; Default

;; Add region to bookmark.

(defcustom bookmark-save-regions t
  "Save and restore regions in bookmark."
  :type 'boolean)

(define-advice bookmark-make-record-default
    (:around (fn &optional no-file no-context posn) region)
  (let ((mark (mark t)))
    `( ,@(funcall fn no-file no-context posn)
       ,@(when (and mark bookmark-save-regions)
           `((mark . ,mark)
             (region-active . ,(region-active-p))
             ,@(unless no-context
                 `((mark-front-context-string
                    . ,(if (>= (- (point-max) mark)
	                       bookmark-search-size)
	                   (buffer-substring-no-properties
	                    mark
                            (+ mark))
	                 nil))))
             ,@(unless no-context
                 `((mark-rear-context-string
                    . ,(if (>= (- mark (point-min))
	                       bookmark-search-size)
	                   (buffer-substring-no-properties
	                    mark
                            (- mark bookmark-search-size))
	                 nil)))))))))

(define-advice bookmark-default-handler (:after (record) region)
  (let ((mark (bookmark-prop-get record 'mark))
        (region-active (bookmark-prop-get record 'region-active))
        (forward-str (bookmark-prop-get record 'mark-rear-context-string))
        (behind-str (bookmark-prop-get record 'mark-front-context-string)))
    (save-excursion
      (and mark
           (goto-char mark))
      (when (and forward-str (search-forward forward-str (point-max) t))
        (goto-char (match-beginning 0)))
      (when (and behind-str (search-backward behind-str (point-min) t))
        (goto-char (match-end 0)))
      (push-mark (point) 'NO-MESSAGE region-active))))

;;:; Dired

(declare-function dired-mark "dired.el")
(declare-function dired-goto-file "dired.el")
(declare-function dired-maybe-insert-subdir "dired.el")
(declare-function dired-omit-mode "dired.el")
(declare-function dired-hide-details-mode "dired.el")
(declare-function dired-unadvertise "dired.el")
(declare-function dired-get-marked-files "dired.el")

(defvar dired-actual-switches)
(defvar dired-subdir-alist)

(defun dired-bookmark-make-record ()
  `( ,@(bookmark-make-record-default)
     (dired-switches . ,dired-actual-switches)
     (dired-marked . ,(dired-get-marked-files nil 'marked))
     (dired-hide-details-mode . ,(bound-and-true-p dired-hide-details-mode))
     (dired-omit-mode . ,(bound-and-true-p dired-omit-mode))
     (dired-directory . ,dired-directory)
     (dired-subdirs . ,(mapcar #'car dired-subdir-alist))
     (mode . ,major-mode)
     (buffer-name . ,(buffer-name))
     (handler . ,#'dired-bookmark-jump)))

;;;###autoload
(defun dired-bookmark-jump (bookmark)
  (interactive (list (bookmark-completing-read* #'dired-bookmark-jump "Jump to bookmark")))
  (let-alist (bookmark-get-bookmark-record bookmark)
    (bookmark-display-buffer (dired-noselect .dired-directory .dired-switches))
    (when (and .buffer-name (not (string-equal .buffer-name (buffer-name))))
      (rename-buffer .buffer-name t)
      (dired-unadvertise default-directory))
    (dired-hide-details-mode (if .dired-hide-details-mode +1 -1))
    (dired-omit-mode (if .dired-omit-mode +1 -1))
    (mapc #'dired-maybe-insert-subdir .dired-subdirs)
    (dolist (file .dired-marked)
      (when (dired-goto-file file)
        (dired-mark nil)))))

;;;###autoload
(defun dired-bookmark-enable ()
  (setq-local bookmark-make-record-function #'dired-bookmark-make-record))

(defun dired-bookmark-upgrade ()
  (when-let* ((name bookmark-current-bookmark)
              ((memq (bookmark-get-handler name) '(nil bookmark-default-handler)))
              (filename (bookmark-get-filename name))
              ((eq major-mode 'dired-mode))
              ((stringp dired-directory))
              ((equal (expand-file-name dired-directory) (expand-file-name filename)))
              ((yes-or-no-p "This is a file bookmark.  Upgrade it to dired bookmark?"))
              ((eq bookmark-make-record-function #'dired-bookmark-make-record)))
    (bookmark-set name)))

(add-hook 'bookmark-after-jump-hook #'dired-bookmark-upgrade)

;;;###autoload(add-hook 'dired-mode-hook #'dired-bookmark-enable)

;;;; EWW

(define-advice eww-bookmark-jump  (:after (bookmark) pos-and-mark)
  (interactive
   (list (bookmark-completing-read*
          #'eww-bookmark-jump "Jump to bookmark")))
  (let ((buf (current-buffer))
        (record (bookmark-get-bookmark-record bookmark)))
    (setq record `(,@record
                   (buffer . ,buf)))
    (letrec ((hook (lambda ()
                     (remove-hook 'eww-after-render-hook hook t)
                     (bookmark-default-handler record))))
      (add-hook 'eww-after-render-hook hook nil t))))

;;;; Mu4e
(declare-function mu4e "ext:mu4e.el")

(defun mu4e-bookmark-main-make-record ()
  "Create a bookmark record for mu4e."
  `("mu4e-main"
    ,@(bookmark-make-record-default 'no-file 'no-context)
    (handler . ,#'mu4e-bookmark-main-handler)))

;;;###autoload
(defun mu4e-bookmark-main-handler (bookmark)
  "Jump to BOOKMARK entry."
  (require 'mu4e)
  (bookmark-display-buffer
   (save-window-excursion
     (mu4e)
     (current-buffer)))
  (goto-char (alist-get 'position bookmark)))

;;;###autoload
(defun mu4e-bookmark-main-enable ()
  "Enable bookmark support for `mu4e-main-mode'."
  (setq-local bookmark-make-record-function
              #'mu4e-bookmark-main-make-record))

;;;; Compilation

(defvar compilation-minor-mode)
(defvar compilation-shell-minor-mode)
(defvar compilation-arguments)

(defun compilation-bookmark-make-record ()
  "Create a bookmark record for compilation mode."
  `( ,@(bookmark-make-record-default 'no-file 'no-context)
     (command . ,(car compilation-arguments))
     (mode . ,(cadr compilation-arguments))
     (highlight-regexp . ,(caddr compilation-arguments))
     (directory . ,default-directory)
     (buffer-name . ,(buffer-name))
     (handler . ,#'compilation-bookmark-jump)))

;;;###autoload
(defun compilation-bookmark-jump (bookmark)
  "Jump to a BOOKMARK entry."
  (interactive
   (list (bookmark-completing-read*
          #'compilation-bookmark-handler
          "Jump to bookmark")))
  (let-alist (bookmark-get-bookmark-record bookmark)
    (let ((default-directory .directory))
      (set-buffer
       (compilation-start
        .command
        .mode
        (lambda (_mode) .buffer-name)
        .highlight-regexp)))))

;;;###autoload
(defalias 'compilation-bookmark-handler #'compilation-bookmark-jump)

;;;###autoload
(defun compilation-bookmark-enable (&optional _)
  "Enable bookmark support for `compilation-mode' and friends."
  (when (or (derived-mode-p 'compilation-mode)
            compilation-shell-minor-mode
            compilation-minor-mode)
    (setq-local bookmark-make-record-function
                #'compilation-bookmark-make-record)))

;;;###autoload(add-hook 'compilation-mode-hook #'compilation-bookmark-enable)

;;;###autoload(add-hook 'compilation-minor-mode-hook #'compilation-bookmark-enable)

;;;###autoload(add-hook 'compilation-shell-minor-mode-hook #'compilation-bookmark-enable)

;;;; Eat-mode

(defvar eat-buffer-name)
(declare-function eat--1 "ext:eat.el")

(defun eat-bookmark-make-record ()
  "Create a bookmark record for `eat-mode'."
  `(,@(bookmark-make-record-default 'no-file 'no-context)
      (default-directory . ,default-directory)
      (buffer-name . ,(buffer-name))
      (handler . ,#'eat-bookmark-jump)))

;;;###autoload
(defun eat-bookmark-jump (bookmark)
  "Jump to a BOOKMARK entry of an Eat buffer."
  (interactive
   (list (bookmark-completing-read*
          #'eat-bookmark-jump
          "Jump to bookmark")))
  (require 'eat)
  (let-alist (bookmark-get-bookmark-record bookmark)
    (let ((default-directory .default-directory)
          (eat-buffer-name .buffer-name))
      (eat--1 nil nil #'bookmark-display-buffer))))

;;;###autoload
(defun eat-bookmark-enable ()
  "Enable bookmark support for Eat."
  (setq-local bookmark-make-record-function #'eat-bookmark-make-record))

;;;###autoload(add-hook 'eat-mode-hook #'eat-bookmark-enable)

;;;; Deadgrep

(defvar deadgrep--initial-filename)
(defvar deadgrep--search-type)
(defvar deadgrep--search-case)
(defvar deadgrep--search-term)
(defvar deadgrep--file-type)
(defvar deadgrep--context)
(defvar deadgrep-display-buffer-function)

(declare-function deadgrep--buffer "ext:deadgrep.el")
(declare-function deadgrep--start "ext:deadgrep.el")
(declare-function deadgrep-next-error "ext:deadgrep.el")
(declare-function deadgrep--create-imenu-index "ext:deadgrep.el")
(declare-function deadgrep--write-heading "ext:deadgrep.el")

(defun deadgrep-bookmark-make-record ()
  "Create a bookmark record for deadgrep buffer."
  `(,@(bookmark-make-record-default 'no-file 'no-context)
    (directory . ,default-directory)
    (initial-filename . ,deadgrep--initial-filename)
    (search-term . ,deadgrep--search-term)
    (search-type . ,deadgrep--search-type)
    (search-case  . ,deadgrep--search-case)
    (file-type . ,deadgrep--file-type)
    (context . ,deadgrep--context)
    (buffer-name . ,(buffer-name))
    (handler . ,#'deadgrep-bookmark-handler)))

;;;###autoload
(defun deadgrep-bookmark-handler (bookmark)
  "Jump to BOOKMARK."
  (require 'deadgrep)
  (let-alist bookmark
    (let* ((buf (deadgrep--buffer
                 .search-term
                 .directory
                 .initial-filename)))
      (bookmark-display-buffer buf)
      (with-current-buffer buf
        (setq imenu-create-index-function #'deadgrep--create-imenu-index)
        (setq next-error-function #'deadgrep-next-error)
        (let ((deadgrep--file-type .file-type)
              (deadgrep--context .context))
          (deadgrep--write-heading)
          (deadgrep--start
           .search-term
           .search-type
           .search-case)))
      buf)))

;;;###autoload
(defun deadgrep-bookmark-enable ()
  "Enable bookmark support for `deadgrep' mode."
  (setq-local bookmark-make-record-function
              #'deadgrep-bookmark-make-record))

;;;###autoload(add-hook 'deadgrep-mode-hook #'deadgrep-bookmark-enable)

;;;; Telega

(defvar telega-root-buffer-name)
(declare-function telega "ext:telega.el")

(defun telega-root-bookmark-make-record ()
  `(,@(bookmark-make-record-default 'no-file 'no-context)
    (buffer-name . ,(buffer-name))
    (handler . telega-root-bookmark-handler)))

;;;###autoload
(defun telega-root-bookmark-handler (bookmark)
  (let ((telega-root-buffer-name (bookmark-prop-get bookmark 'buffer-name)))
    (bookmark-display-buffer (telega 'no-pop))))

;;;###autoload
(defun telega-root-bookmark-enable ()
  (setq-local bookmark-make-record-function #'telega-root-bookmark-make-record))

;;;###autoload(add-hook 'telega-root-mode-hook #'telega-root-bookmark-enable)

(declare-function telega-chatbuf--get-create "ext:telega-chat.el")
(declare-function telega-chat-get "ext:telega-chat.el")

(defun telega-chat-bookmark-make-record ()
  (defvar telega-chatbuf--chat)
  `(,@(bookmark-make-record-default 'no-file)
      (buffer-name . ,(buffer-name))
      (telega-chat-id . ,(plist-get telega-chatbuf--chat :id))
      (handler . telega-chat-bookmark-handler)))

;;;###autoload
(defun telega-chat-bookmark-handler (bookmark)
  (require 'telega)
  (telega t)
  (let* ((id (bookmark-prop-get bookmark 'telega-chat-id))
         (chat (telega-chat-get id)))
    (bookmark-display-buffer
     (telega-chatbuf--get-create chat))))

;;;###autoload
(defun telega-chat-bookmark-enable ()
  (setq-local bookmark-make-record-function #'telega-chat-bookmark-make-record))

;;;###autoload(add-hook 'telega-chat-mode-hook #'telega-chat-bookmark-enable)

;;;###autoload
(defun telega-bookmark-install ()
  (add-hook 'telega-root-mode-hook #'telega-root-bookmark-enable)
  (add-hook 'telega-chat-mode-hook #'telega-chat-bookmark-enable))

;;;; Browser independent URL bookmark

;;;###autoload
(defun url-bookmark-jump (bookmark)
  (let ((pos (bookmark-prop-get bookmark 'location)))
    (pcase browse-url-browser-function
      ('eww-browse-url (eww-bookmark-jump bookmark))
      (fn
       (require 'browse-url)
       (set-buffer (get-buffer-create (format " *browse-url : %s*" pos)))
       (funcall fn pos)))))

(put 'url-bookmark-jump 'bookmark-handler-type "URL")

;;;###autoload
(defun url-bookmark-add ()
  (interactive)
  (let ((url (read-string "URL: " (ffap-url-at-point)))
        (name (read-string "Name: ")))
    (let ((bookmark `((location . ,url)
                      (handler . ,#'url-bookmark-jump))))
      (bookmark-store name bookmark t))))

;;;; Org link bookmark

(declare-function org-insert-link-global "ol.el")
(declare-function org-link-open-from-string "ol.el")
(declare-function org-element-property "org-element-ast.el")
(declare-function org-element-put-property "org-element-ast.el")
(declare-function org-element-link-parser "org-element.el")

;;;###autoload
(defun org-link-bookmark-jump (bookmark)
  (interactive
   (list (bookmark-completing-read*
          #'org-link-bookmark-jump
          "Jump to bookmark")))
  (require 'ol)
  (defvar org-link-elisp-confirm-function)
  (defvar org-link-frame-setup)
  (prog1
      (cl-letf ((link (bookmark-prop-get bookmark 'org-link))
                (org-link-elisp-confirm-function #'always)
                ((alist-get 'file org-link-frame-setup)
                 (lambda (file)
                   (bookmark-display-buffer (find-file-noselect file)))))
        (org-link-open-from-string link))
    ;; bookmark-jump runs the bookmark handler with
    ;; `save-window-excursion', so we need to save the window
    ;; configuration and restore it in `bookmark-after-jump-hook'.
    ;; The same idiom is used by `bookmark-view' and pdf-tools.  See
    ;; also
    ;; [[https://lists.gnu.org/archive/html/emacs-devel/2022-08/msg00500.html]].
    (let ((wind-conf (current-window-configuration)))
      (letrec ((hook (lambda ()
                       (setq bookmark-after-jump-hook (delq hook bookmark-after-jump-hook))
                       (set-window-configuration wind-conf))))
        (push hook bookmark-after-jump-hook)))))

(defun org-link-bookmark--parse-link (link-string)
  (require 'org-element)
  (require 'org-element-ast)
  (with-temp-buffer
    (insert link-string)
    (org-mode)
    (goto-char (point-min))
    (let ((link (org-element-link-parser)))
      (when-let* ((beg (org-element-property :contents-begin link))
                  (end (org-element-property :contents-end link)))
        (org-element-put-property link :contents (buffer-substring-no-properties beg end)))
      link)))

(defun org-link-bookmark--read-link ()
  (with-temp-buffer
    (defvar org-link-file-path-type)
    (let ((org-link-file-path-type 'absolute))
      (org-insert-link-global))
    (buffer-string)))

;;;###autoload
(defun org-link-bookmark-set (link-string &optional no-overwrite)
  "Create a bookmark for an Org link.

The bookmark stores the link string and allows jumping to it using
`bookmark-jump'.  The bookmark record includes the link's type and
path, and for file links the filename, for URL links the location.

LINK-STRING is an Org link string such as \"[[file:~/doc.org][Doc]]\".
With optional prefix argument NO-OVERWRITE, do not overwrite an
existing bookmark with the same name."
  (interactive (list (org-link-bookmark--read-link) current-prefix-arg))
  (require 'ol)
  (let* ((link (org-link-bookmark--parse-link link-string))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (desc (org-element-property :contents link))
         (name (or desc
                   (let ((default (concat type ":" path)))
                     (read-string (format-prompt "Bookmark name" default)
                                  nil nil default))))
         (record `((org-link . ,link-string)
                   ,@(when (string= type "file")
                       `((filename . ,path)))
                   ,@(if (member type '("http" "https" "ftp"))
                         `((location . ,(concat type ":" path))
                           (handler . ,#'url-bookmark-jump))
                       `((handler . ,#'org-link-bookmark-jump))))))
    (bookmark-store name record no-overwrite)))

(defvar embark-target-injection-hooks)
(declare-function embark--allow-edit "ext:embark.el")
(with-eval-after-load 'embark
  (cl-pushnew #'embark--allow-edit
              (alist-get 'org-link-bookmark-set
                         embark-target-injection-hooks)))


;;;###autoload
(defun bookmark-extras-install ()
  (interactive)
  (add-hook 'dired-mode-hook #'dired-bookmark-enable)
  (add-hook 'compilation-mode-hook #'compilation-bookmark-enable)
  (add-hook 'compilation-minor-mode-hook #'compilation-bookmark-enable)
  (add-hook 'compilation-shell-minor-mode-hook #'compilation-bookmark-enable)
  (add-hook 'eat-mode-hook #'eat-bookmark-enable)
  (add-hook 'deadgrep-mode-hook #'deadgrep-bookmark-enable)
  (telega-bookmark-install))


(provide 'bookmark-extras)
;;; bookmark-extras.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
