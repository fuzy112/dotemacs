;;; bookmark-extras.el --- Bookmark support for various modes -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu

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

;; This Emacs package, `bookmark-extras.el', provides bookmark support
;; for various modes.  It defines functions for creating bookmark
;; records and handlers for jumping to bookmarked entries in each
;; mode.  Additionally, it sets up autoloads and enables bookmark
;; functionality for the corresponding modes.

;; Currently supported modes:
;;  - devdocs-mode
;;  - mu4e-main-mode
;;  - compilation-mode
;;  - shell-mode
;;  - eat-mode
;;  - deadgrep-mode
;;
;; An browser independent URL bookmark type is also supported.
;;
;; This package also debounces call to `bookmark-save' in
;; `bookmark-save-debounce-functions'.

;;; Code:

(require 'bookmark)

(eval-when-compile
  (require 'let-alist))

(defgroup bookmark-extras ()
  "Enhancements to bookmark functionality."
  :group 'bookmark)

;;;; Bookmark-save debounce

;; When using `bookmark-delete' etc. with `embark-act-all', the
;; bookmark file is saved many times, which is very slow.  The
;; following code debounces the `bookmark-save' call, in the specified
;; functions, to optimise the performance.

(defun bookmark-save-debounce-functions--set (symbol value)
  (when (eq symbol 'bookmark-save-debounce-functions)
    (when (boundp 'bookmark-save-debounce-functions)
      (dolist (fn bookmark-save-debounce-functions)
        (advice-remove fn #'bookmark-save-debounce)))
    (set-default symbol value)
    (dolist (fn value)
      (advice-add fn :around #'bookmark-save-debounce))))

(defcustom bookmark-save-debounce-functions
  '(bookmark-store
    bookmark-relocate
    bookmark-rename
    bookmark-delete
    bookmark-delete-all)
  "Functions in which `bookmark-save' is debounced."
  :type '(repeat function)
  :set #'bookmark-save-debounce-functions--set)

(defcustom bookmark-save-delay 1.5
  "The number of seconds to wait before running `bookmark-save'."
  :type 'number)

(defvar bookmark-save-debounce-timer nil)

(defun bookmark-save-debounce (&rest args)
  (let ((bookmark-save-flag t))
    (prog1
        (apply args)
      (when (timerp bookmark-save-debounce-timer)
        (cancel-timer bookmark-save-debounce-timer))
      (setq bookmark-save-debounce-timer
            (run-with-timer bookmark-save-delay nil
                            #'bookmark-save)))))

(dolist (fn bookmark-save-debounce-functions)
  (advice-add fn :around #'bookmark-save-debounce))

;;;; DevDocs

(defvar devdocs--stack)
(declare-function devdocs--render "ext:devdocs")

(defun devdocs-bookmark-make-record ()
  "Create a bookmark record."
  (let ((entry (car devdocs--stack)))
    `(,(format "DevDocs: %s" (alist-get 'name entry))
      ,@(bookmark-make-record-default 'no-file 'no-context)
      (entry . ,entry)
      (handler . ,#'devdocs-bookmark-handler))))

;;;###autoload
(defun devdocs-bookmark-handler (bookmark)
  "Jump to BOOKMARK entry."
  (require 'devdocs)
  (let-alist bookmark
    (let ((buf (devdocs--render .entry)))
      (set-buffer buf)
      (goto-char .position))))

;;;###autoload
(defun devdocs-bookmark-enable ()
  "Enable devdocs-bookmark support."
  (setq-local bookmark-make-record-function
              #'devdocs-bookmark-make-record))

;;;###autoload(add-hook 'devdocs-mode-hook #'devdocs-bookmark-enable)

;;;; Mu4e
(declare-function mu4e "ext:mu4e")

(defun mu4e-bookmark-main-make-record ()
  "Create a bookmark record for mu4e."
  `("mu4e-main"
    ,@(bookmark-make-record-default 'no-file 'no-context)
    (handler . ,#'mu4e-bookmark-main-handler)))

;;;###autoload
(defun mu4e-bookmark-main-handler (bookmark)
  "Jump to BOOKMARK entry."
  (require 'mu4e)
  (set-buffer
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
  `(,@(bookmark-make-record-default 'no-file 'no-context)
    (compilation-arguments . ,compilation-arguments)
    (directory . ,default-directory)
    (major-mode . ,major-mode)
    (compilation-minor-mode . ,compilation-minor-mode)
    (compilation-shell-minor-mode . ,compilation-shell-minor-mode)
    (buffer-name . ,(buffer-name))
    (handler . ,#'compilation-bookmark-handler)))

;;;###autoload
(defun compilation-bookmark-handler (bookmark)
  "Jump to a BOOKMARK entry."
  (let-alist bookmark
    (with-current-buffer (get-buffer-create .buffer-name)
      (setq default-directory .directory)
      (funcall .major-mode)
      (setq-local compilation-arguments .compilation-arguments)
      (when compilation-minor-mode
        (compilation-minor-mode +1))
      (when compilation-shell-minor-mode
        (compilation-shell-minor-mode +1))
      (apply #'compilation-start compilation-arguments))))

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

;;;; Shell-Mode

(defun shell-bookmark-make-record ()
  "Create a bookmark record."
  `(,@(bookmark-make-record-default 'no-file 'no-context)
    (default-directory . ,default-directory)
    (buffer-name . ,(buffer-name))
    (handler . ,#'shell-bookmark-handler)))

;;;###autoload
(defun shell-bookmark-handler (bookmark)
  "Jump to a BOOKMARK entry."
  (let-alist bookmark
    (let ((default-directory .default-directory))
      (shell .buffer-name))))

;;;###autoload
(defun shell-bookmark-enable ()
  "Enable bookmark support for `shell-mode'."
  (setq-local bookmark-make-record-function
              #'shell-bookmark-make-record))

;;;###autoload(add-hook 'shell-mode-hook #'shell-bookmark-enable)

;;;; Eat-mode

(defvar eat-buffer-name)

(defun eat-bookmark-make-record ()
  "Create a bookmark record for `eat-mode'."
  `(,@(bookmark-make-record-default 'no-file 'no-context)
    (default-directory . ,default-directory)
    (buffer-name . ,(buffer-name))
    (handler . ,#'eat-bookmark-handler)))

;;;###autoload
(defun eat-bookmark-handler (bookmark)
  "Jump to a BOOKMARK entry of an Eat buffer."
  (require 'eat)
  (let-alist bookmark
    (let ((default-directory .default-directory)
          (eat-buffer-name .buffer-name))
      (set-buffer (eat)))))

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

(declare-function deadgrep--buffer "deadgrep")
(declare-function deadgrep--start "deadgrep")
(declare-function deadgrep-next-error "deadgrep")
(declare-function deadgrep--create-imenu-index "deadgrep")
(declare-function deadgrep--write-heading "deadgrep")

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
      (funcall deadgrep-display-buffer-function buf)
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

(defun telega-root-bookmark-make-record ()
  `(,@(bookmark-make-record-default 'no-file 'no-context)
    (buffer-name . ,(buffer-name))
    (handler . telega-root-bookmark-handler)))

;;;###autoload
(defun telega-root-bookmark-handler (bookmark)
  (let ((telega-root-buffer-name (bookmark-prop-get bookmark 'buffer-name)))
    (set-buffer (telega))))

;;;###autoload
(defun telega-root-bookmark-enable ()
  (setq-local bookmark-make-record-function #'telega-root-bookmark-make-record))

;;;###autoload(add-hook 'telega-root-mode-hook #'telega-root-bookmark-enable)

(defun telega-chat-bookmark-make-record ()
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
    (set-buffer
     (telega-chatbuf--get-create chat))))

;;;###autoload
(defun telega-chat-bookmark-enable ()
  (setq-local bookmark-make-record-function #'telega-chat-bookmark-make-record))

;;;###autoload(add-hook 'telega-chat-mode-hook #'telaga-chat-bookmark-enable)

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

;;;###autoload
(defun bookmark-extras-install ()
  (interactive)
  (add-hook 'devdocs-mode-hook #'devdocs-bookmark-enable)
  (add-hook 'compilation-mode-hook #'compilation-bookmark-enable)
  (add-hook 'compilation-minor-mode-hook #'compilation-bookmark-enable)
  (add-hook 'compilation-shell-minor-mode-hook #'compilation-bookmark-enable)
  (add-hook 'shell-mode-hook #'shell-bookmark-enable)
  (add-hook 'eat-mode-hook #'eat-bookmark-enable)
  (add-hook 'deadgrep-mode-hook #'deadgrep-bookmark-enable)
  (telega-bookmark-install))


(provide 'bookmark-extras)
;;; bookmark-extras.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
