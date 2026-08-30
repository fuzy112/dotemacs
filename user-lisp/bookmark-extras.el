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

;; This Emacs package, `bookmark-extras.el', enhances the standard
;; bookmark system by providing bookmark support for various modes
;; and defining new bookmark types.  It includes functions for
;; creating bookmark records and handlers for jumping to bookmarked
;; entries.
;;
;; Supported modes:
;;  - dired-mode
;;  - eww-mode (via advice on `eww-bookmark-jump')
;;  - mu4e-main-mode
;;  - compilation-mode (including compilation-minor-mode and
;;    compilation-shell-minor-mode)
;;  - eat-mode
;;  - deadgrep-mode
;;  - telega-root-mode and telega-chat-mode
;;
;; Additionally, it provides:
;;  - A browser‑independent URL bookmark type (`url-bookmark-jump' /
;;    `url-bookmark-add')
;;  - An Org link bookmark type (`org-link-bookmark-jump' /
;;    `org-link-bookmark-set')
;;
;; Use `bookmark-extras-install' to enable all modes at once, or
;; activate each mode individually by adding the appropriate enable
;; function to its mode hook.

;;; Code:

(require 'bookmark)
(require 'map)
(eval-when-compile
  (require 'let-alist))

(defgroup bookmark-extras ()
  "Enhancements to bookmark functionality."
  :group 'bookmark)

;;;; Utility

(defun bookmark-display-buffer (buffer &optional bookmark)
  "Display BUFFER, optionally using BOOKMARK's display settings.
If `bmkp-jump-display-function' is bound and non-nil, call it with
BUFFER and then switch to the buffer.  Otherwise, use
`bookmark-default-handler' with a bookmark record constructed from
BUFFER and BOOKMARK."
  (cond
   ((bound-and-true-p bmkp-jump-display-function)
    (funcall bmkp-jump-display-function buffer)
    (set-buffer buffer))
   (t
    (bookmark-default-handler
     `(""
       (buffer . ,buffer)
       ,@(map-remove
          (lambda (key _) (eq key 'filename))
          (bookmark-get-bookmark-record bookmark)))))))

(defun bookmark-completing-read* (handler prompt &optional default)
  "Read a bookmark name with completion, filtering by HANDLER.

HANDLER is a symbol or list of symbols; only bookmarks whose `handler'
property matches are offered.  PROMPT is the minibuffer prompt.  DEFAULT
is the default bookmark name; if nil or empty, the first bookmark in
`bookmark-alist' is used.  If the user enters an empty string, signal
`user-error'."
  (minibuffer-with-setup-hook
      (:append
       (lambda ()
         (setq-local minibuffer-completion-predicate
                     (lambda (cand)
                       (memq (alist-get 'handler (cdr cand))
                             (ensure-list handler))))))
    (when (or (null default) (string-empty-p default))
      (bookmark-maybe-load-default-file)
      (setq default (caar bookmark-alist)))
    (let ((selected (bookmark-completing-read prompt default)))
      (when (string-empty-p selected)
        (user-error "User selected nothing"))
      selected)))

;;;; Default

;; Add region to bookmark.

(defcustom bookmark-save-regions t
  "Save and restore regions in bookmark."
  :type 'boolean)

(define-advice bookmark-make-record-default
    (:around (fn &optional no-file no-context posn) region)
  "Add region information to the default bookmark record.
When `bookmark-save-regions' is non-nil, this advice appends
the current mark position, region activation state, and context
strings (if not in no-context mode) to the bookmark record
created by `bookmark-make-record-default'."
  `( ,@(funcall fn no-file no-context posn)
     ,@(when-let* ((mark (mark t))
                   ((and bookmark-save-regions
                         (or (not posn) (= posn (point))))))
         `((mark . ,mark)
           (region-active . ,(region-active-p))
           ,@(unless no-context
               `((mark-front-context-string
                  . ,(if (>= (- (point-max) mark)
	                     bookmark-search-size)
	                 (buffer-substring-no-properties
	                  mark
                          (+ mark bookmark-search-size))
	               nil))))
           ,@(unless no-context
               `((mark-rear-context-string
                  . ,(if (>= (- mark (point-min))
	                     bookmark-search-size)
	                 (buffer-substring-no-properties
	                  mark
                          (- mark bookmark-search-size))
	               nil))))))))

(define-advice bookmark-default-handler (:after (record) region)
  "Restore the mark and region after jumping to a bookmark."
  (let ((mark (bookmark-prop-get record 'mark))
        (region-active (bookmark-prop-get record 'region-active))
        (forward-str (bookmark-prop-get record 'mark-front-context-string))
        (behind-str (bookmark-prop-get record 'mark-rear-context-string)))
    (save-excursion
      (when mark
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
     (dired-switches          . ,dired-actual-switches)
     (dired-marked            . ,(dired-get-marked-files nil 'marked))
     (dired-hide-details-mode . ,(bound-and-true-p dired-hide-details-mode))
     (dired-omit-mode         . ,(bound-and-true-p dired-omit-mode))
     (dired-directory         . ,dired-directory)
     (dired-subdirs           . ,(mapcar #'car dired-subdir-alist))
     (mode                    . ,major-mode)
     (buffer-name             . ,(buffer-name))
     (handler                 . ,#'dired-bookmark-jump)))

;;;###autoload
(defun dired-bookmark-jump (bookmark)
  "Jump to a dired bookmark.

Display the directory and restore the saved switches, subdirectories,
hidden-details mode, omit mode, and marked files as saved in the
bookmark.

Interactively, prompt for a bookmark name using completion limited to
dired bookmarks."
  (interactive (list (bookmark-completing-read* #'dired-bookmark-jump "Jump to bookmark")))
  (let-alist (bookmark-get-bookmark-record bookmark)
    (bookmark-display-buffer (dired-noselect .dired-directory .dired-switches))
    (when (and .buffer-name (not (string-equal .buffer-name (buffer-name))))
      (rename-buffer .buffer-name t)
      (dired-unadvertise default-directory))
    (when (xor (bound-and-true-p dired-hide-details-mode)
               .dired-hide-details-mode)
      (dired-hide-details-mode (if .dired-hide-details-mode +1 -1)))
    (when (xor (bound-and-true-p dired-omit-mode) .dired-omit-mode)
      (dired-omit-mode (if .dired-omit-mode +1 -1)))
    (mapc #'dired-maybe-insert-subdir .dired-subdirs)
    (dolist (file .dired-marked)
      (when (dired-goto-file file)
        (dired-mark nil)))))

;;;###autoload
(defun dired-bookmark-enable ()
  (setq-local bookmark-make-record-function #'dired-bookmark-make-record))

(defun dired-bookmark-upgrade ()
  "Upgrade a file bookmark to a dired bookmark if appropriate.
This function is intended to be added to `bookmark-after-jump-hook'.
When invoked after jumping to a bookmark, it checks if the bookmark
is a simple file bookmark (i.e., using `bookmark-default-handler' or
nil handler) whose filename matches the current `dired-directory'
in a Dired buffer.  If so, it prompts the user to upgrade the bookmark
to use `dired-bookmark-make-record' as the bookmark creation function,
so that later jumps will restore the Dired state correctly."
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

(define-advice eww-bookmark-jump (:after (record) pos-and-mark)
  "Restore point and mark after EWW bookmark page loads.
Also allows interactive bookmark selection."
  (interactive
   (list (bookmark-get-bookmark
          (bookmark-completing-read*
           '(eww-bookmark-jump
             url-bookmark-jump
             xwidget-webkit-bookmark-jump-handler)
           "Jump to record"))))
  (let ((buf (current-buffer)))
    (letrec ((hook (lambda ()
                     (remove-hook 'eww-after-render-hook hook t)
                     (bookmark-display-buffer buf record)
                     (when bookmark-fringe-mark
                       (bookmark--set-fringe-mark))
                     (run-hooks 'bookmark-after-jump-hook))))
      (add-hook 'eww-after-render-hook hook nil t))))

;;;; Help

(define-advice help-bookmark-jump (:after (record) restore-point)
  (interactive
   (list (bookmark-get-bookmark
          (bookmark-completing-read*
           '(help-bookmark-jump)
           "Jump to bookmark"))))
  (bookmark-display-buffer (current-buffer) record))

;;;; Xwidget webkit

(put 'xwidget-webkit-bookmark-jump-handler 'bookmark-handler-type "Xwidget")

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
     (command          . ,(car compilation-arguments))
     (mode             . ,(cadr compilation-arguments))
     (highlight-regexp . ,(caddr compilation-arguments))
     (filename         . ,default-directory)
     (buffer-name      . ,(buffer-name))
     (handler          . ,#'compilation-bookmark-jump)))

;;;###autoload
(defun compilation-bookmark-jump (bookmark)
  "Jump to a compilation bookmark.
Interactively, prompt for a bookmark using `bookmark-completing-read*'."
  (interactive
   (list (bookmark-completing-read*
          #'compilation-bookmark-jump
          "Jump to bookmark")))
  (let ((default-directory (or (bookmark-prop-get bookmark 'filename)
                               (bookmark-prop-get bookmark 'directory))))
    (bookmark-display-buffer
     (compilation-start (bookmark-prop-get bookmark 'command)
                        (bookmark-prop-get bookmark 'mode)
                        (let ((buffer-name (bookmark-prop-get bookmark 'buffer-name)))
                          (lambda (_) buffer-name))
                        (bookmark-prop-get bookmark 'highlight-regexp)))))

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
  `( ,@(bookmark-make-record-default 'no-file 'no-context)
     (filename . ,default-directory)
     (buffer-name . ,(buffer-name))
     (handler . ,#'eat-bookmark-jump)))

;;;###autoload
(defun eat-bookmark-jump (bookmark)
  "Jump to a bookmark in an Eat buffer.
Interactively, prompt for a bookmark to jump to using completion."
  (interactive
   (list (bookmark-completing-read*
          #'eat-bookmark-jump
          "Jump to bookmark")))
  (require 'eat)
  (let* ((default-directory
          (or (bookmark-prop-get bookmark 'filename)
              (bookmark-prop-get bookmark 'default-directory)))
         (buffer-name (bookmark-prop-get bookmark 'buffer-name))
         (buffer (eat--1 nil nil #'ignore)))
    (with-current-buffer buffer
      (rename-buffer buffer-name 'UNIQUE))
    (bookmark-display-buffer buffer bookmark)))

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
    (directory        . ,default-directory)
    (initial-filename . ,deadgrep--initial-filename)
    (search-term      . ,deadgrep--search-term)
    (search-type      . ,deadgrep--search-type)
    (search-case      . ,deadgrep--search-case)
    (file-type        . ,deadgrep--file-type)
    (context          . ,deadgrep--context)
    (buffer-name      . ,(buffer-name))
    (handler          . ,#'deadgrep-bookmark-handler)))

;;;###autoload
(defun deadgrep-bookmark-handler (bookmark)
  "Jump to BOOKMARK."
  (require 'deadgrep)
  (let-alist bookmark
    (let* ((buf (deadgrep--buffer .search-term .directory .initial-filename)))
      (with-current-buffer buf
        (setq imenu-create-index-function #'deadgrep--create-imenu-index)
        (setq next-error-function #'deadgrep-next-error)
        (let ((deadgrep--file-type .file-type)
              (deadgrep--context .context))
          (deadgrep--write-heading)
          (deadgrep--start .search-term .search-type .search-case)))
      (bookmark-display-buffer buf))))

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
  "Jump to a URL bookmark using the configured browser.
When called interactively, prompt for a bookmark among those
compatible with `url-bookmark-jump', `eww-bookmark-jump', or
`xwidget-webkit-bookmark-jump-handler'."
  (interactive
   (list (bookmark-get-bookmark
          (bookmark-completing-read*
           '(url-bookmark-jump
             eww-bookmark-jump
             xwidget-webkit-bookmark-jump-handler)
           "Jump to bookmark"))))
  (let ((pos (bookmark-prop-get bookmark 'location)))
    (pcase browse-url-browser-function
      ('eww-browse-url (eww-bookmark-jump bookmark))
      (fn
       (require 'browse-url)
       (set-buffer (get-buffer-create (format " *browse-url : %s*" pos)))
       (let ((wincfg (current-window-configuration)))
         (letrec ((fun (lambda ()
                         (remove-hook 'bookmark-after-jump-hook fun)
                         (set-window-configuration wincfg))))
           (funcall fn pos)
           (add-hook 'bookmark-after-jump-hook fun wincfg)))))))

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
  "Jump to an Org link stored in a bookmark.
When called interactively, prompt for a bookmark using
`bookmark-completing-read*' and jump to the link stored in it."
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
                   (bookmark-display-buffer (find-file-noselect file) bookmark))))
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
                   (location . ,(concat type ":" path))
                   ,@(when (string= type "file")
                       `((filename . ,path)))
                   ,@(if (member type '("http" "https" "ftp"))
                         `((handler . ,#'url-bookmark-jump))
                       `((handler . ,#'org-link-bookmark-jump))))))
    (bookmark-store name record no-overwrite)))

;;;; Editing bookmark record

(defvar-keymap bookmark-edit-bookmark-record-mode-map
  "C-c C-c" #'bookmark-edit-bookmark-record-finish
  "C-c C-k" #'kill-current-buffer)

(define-minor-mode bookmark-edit-bookmark-record-mode
  "Minor mode for editing a bookmark record in a dedicated buffer.

\\{bookmark-edit-bookmark-record-mode-map}"
  :keymap bookmark-edit-bookmark-record-mode-map
  :interactive nil)

(defvar-local bookmark-edit-bookmark-name nil
  "Name of the bookmark being edited in the current buffer.")

(defun bookmark-edit-bookmark-record (bookmark)
  "Edit the record of BOOKMARK in a new buffer.

Interactively, read a bookmark name with completion,
defaulting to `bookmark-current-bookmark'.

\\<bookmark-edit-bookmark-record-mode-map>
The buffer is set up in `lisp-data-mode' with the bookmark
record pretty-printed.  Save changes with \\[bookmark-edit-bookmark-record-finish]
or abort with \\[kill-current-buffer]."
  (interactive
   (list
    (bookmark-completing-read
     "Edit bookmark record: "
     (list bookmark-current-bookmark))))
  (let ((record (bookmark-get-bookmark-record bookmark))
        (buf (generate-new-buffer (format "*Bookmark Edit: %s*" bookmark))))
    (with-current-buffer buf
      (lisp-data-mode)
      (bookmark-edit-bookmark-record-mode)
      (let ((hint (substitute-command-keys
                   "Type \\[bookmark-edit-bookmark-record-finish] when done. \
Type \\[kill-current-buffer] to cancel.")
                  ))
        (insert ";; -*- mode: lisp-data; coding: utf-8-emacs; -*-\n\n"
                ";; You are editing the bookmark record for bookmark ‘"
                bookmark "’.\n"
                ";; " hint)
        (fill-comment-paragraph)
        (newline 2)
        (setq-local header-line-format hint))
      (let ((pp-default-function #'pp-28))
        (pp record (current-buffer)))
      (newline)
      (setq-local bookmark-edit-bookmark-name bookmark))
    (pop-to-buffer buf)))

(defun bookmark-edit-bookmark-record-finish ()
  "Finish editing the bookmark record.

Read the contents of the current buffer starting from the first
non-comment, non-whitespace sexp and store it as the new record
for the bookmark named by `bookmark-edit-bookmark-name'."
  (interactive nil bookmark-edit-bookmark-record-mode)
  (bookmark-store bookmark-edit-bookmark-name
                  (save-excursion
                    (goto-char (point-min))
                    (read (current-buffer)))
                  nil)
  (quit-window))

(keymap-set bookmark-map "E" #'bookmark-edit-bookmark-record)



(defvar embark-target-injection-hooks)
(defvar embark-bookmark-map)
(declare-function embark--allow-edit "ext:embark.el")
(with-eval-after-load 'embark
  (cl-pushnew #'embark--allow-edit
              (alist-get 'org-link-bookmark-set
                         embark-target-injection-hooks))

  (keymap-set embark-bookmark-map "m" 'bookmark-edit-bookmark-record))


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
