;;; medit.el --- Edit multiple matches of the same text -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.11.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

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

;; This package provides an alternative to VSCode's "Edit all
;; occurrences" functionality.

;; To use this package, simply bind `medit-dwim' to your favorite key,
;; then move point to a symbol that you want to edit.  Press the key
;; to activate a medit session.  Once you are done, press `RET' to
;; apply the result or `C-g' to cancel the edit.

;; An example configuration is like this:
;;
;;  (keymap-global-set "M-s %" #'medit-dwim)
;;  (keymap-set isearch-mode-map "M-s %" #'medit-from-isearch))
;;  (with-eval-after-load 'embark
;;    (keymap-set embark-identifier-map "%" #'medit))
;;

;;; Code:

(require 'compat nil t)
(eval-when-compile
  (require 'cl-lib))

(defgroup medit nil
  "Match edit."
  :group 'convenience
  :prefix "medit-")

;;;; Faces

(defface medit-match '((t :inherit lazy-highlight))
  "Face for displaying match items.")

(defface medit-region '((t :inherit isearch))
  "Face for displaying the current region being edited.")

;;;; Variables

(defcustom medit-style-alist
  `((plain . ,#'medit--plain-match)
    (word . ,#'medit--word-match)
    (symbol . ,#'medit--symbol-match))
  "An alist of pairs of symbol and functions.
Keys of the alist can be used as values of `medit-style'.  Values
of the alist are function, which, when passed a string, returns a
regexp that matches the string, and, when passed nil, returns
either a function that can be used to find the default string to
match for, or nil, to indicate that the default string is to be
read from the minibuffer."
  :type '(alist :key-type symbol
                :value-type function))

(defcustom medit-default-style 'symbol
  "The default matching style to use.
See also `medit-style-alist'."
  :type '(choice (const plain)
                 (const word)
                 (const symbol)
                 (symbol :tag "Other")))

(defcustom medit-use-minibuffer nil
  "Run medit in minibuffer."
  :type 'boolean)

(defvar medit-string nil
  "The string to edit.")

(defvar medit-style nil
  "The current matching style.
The value should be either nil, or one of `plain', `word', and
`symbol'.")

;;;; Overlay categories

(put 'medit-match 'face 'medit-match)
(put 'medit-match 'priority 1)

(put 'medit-region 'face 'medit-region)
(put 'medit-region 'priority 2)

;;;; Internal variables

(defvar medit--edit-overlay
  (let ((ol (make-overlay 0 0 nil nil t)))
    (overlay-put ol 'category 'medit-region)
    (overlay-put ol 'modification-hooks '(medit--in-buffer-modification-hook))
    (overlay-put ol 'insert-in-fronk-hooks '(medit--in-buffer-modification-hook))
    (overlay-put ol 'insert-behind-hooks '(medit--in-buffer-modification-hook))
    (delete-overlay ol)
    ol))

(defvar medit--bounds nil)

(defvar medit--matches nil)

(defvar medit--in-buffer-modified nil)

;;;; Keymap

(defvar-keymap medit-map
  :doc "Keymap used in medit minibuffer settion."
  :parent minibuffer-local-map
  "C-r" #'medit-previous
  "C-s" #'medit-next
  "RET" #'medit-replace
  "C-g" #'exit-minibuffer
  "M-s _" #'medit-toggle-symbol
  "M-s w" #'medit-toggle-word
  "<remap> <medit-dwim>" #'abort-minibuffers)

;;;; Internal functions

(defun medit--plain-match (str)
  (cond (str (regexp-quote str))
        (t nil)))

(defun medit--word-match (str)
  (cond (str (regexp-opt (list str) 'words))
        (t (thing-at-point 'word))))

(defun medit--symbol-match (str)
  (cond (str (regexp-opt (list str) 'symbols))
        (t (thing-at-point 'symbol))))

(defun medit--regexp (str)
  "Create ragexp that matches STR according to `medit-style'."
  (let ((regexp-fun (alist-get medit-style medit-style-alist)))
    (funcall regexp-fun str)))

(defun medit--string ()
  "Return the default text to match."
  (let ((match-fun (alist-get medit-style medit-style-alist)))
    (funcall match-fun nil)))

(defun medit--setup-keymap ()
  "Setup the keymap used in the minibuffer."
  (use-local-map
   (make-composed-keymap
    (list medit-map)
    (current-local-map))))


(defun medit--make-overlays (start end)
  "Find matches of `medit-string' and create overlays.
If START and END are not-nil, the search will be limited to that
region."
  (when medit-string
    (save-excursion
      (goto-char (or start (point-min)))
      (let ((regexp (medit--regexp medit-string))
            overlays)
	(while (re-search-forward regexp end t)
	  (when (or medit-use-minibuffer
		    (not (memq medit--edit-overlay (overlays-at (match-beginning 0)))))
            (let ((match
		   (make-overlay (match-beginning 0) (match-end 0) nil nil t)))
              (overlay-put match 'category 'medit-match)
              (push match overlays))))
	overlays))))

(defun medit--minibuffer-after-change-function (&rest _)
  "Function to call after minibuffer content change."
  (let ((replacement (minibuffer-contents-no-properties)))
    (overlay-put medit--edit-overlay
                 'display
                 (propertize replacement 'face 'medit-region))
    (put 'medit-match 'display
         (propertize replacement 'face 'medit-match))))

(defun medit--find-matches ()
  (mapc #'delete-overlay medit--matches)
  (setq medit--matches (medit--make-overlays
                        (and medit--bounds (car medit--bounds))
                        (and medit--bounds (cdr medit--bounds)))))

(defun medit--find-regexp-at-point (regexp)
  "Find a match of REGEXP at point."
  (if (looking-at regexp)
      ;; Return directly if we are looking-at the pattern.
      (point)
    (save-excursion
      ;; Otherwise, search for the next pattern.
      (if (re-search-forward regexp nil 'no-error)
          ;; If found, go to beginning of the match.
          (goto-char (match-beginning 0)))
      ;; Search for the match at point
      (re-search-backward regexp nil))))

(defun medit--overlay-text ()
  (buffer-substring-no-properties
   (overlay-start medit--edit-overlay)
   (overlay-end medit--edit-overlay)))

(defun medit--in-buffer-modification-hook (_overlay changed &rest _)
  (when (and (not medit-use-minibuffer) changed)
    (setq medit--in-buffer-modified t)
    (let ((new-str (medit--overlay-text)))
      (put 'medit-match 'display
	   (propertize new-str 'face 'medit-match)))))

;;;; Public functions

(defun medit-reset-vars ()
  "Reset internal variables used by medit."
  (setq medit-string nil)
  (setq medit-style nil)
  (mapc #'delete-overlay medit--matches)
  (setq medit--matches nil)
  (setq medit--bounds nil)
  (setq medit--in-buffer-modified nil)
  (overlay-put medit--edit-overlay 'display nil)
  (delete-overlay medit--edit-overlay)
  (put 'medit-match 'display nil))

(defun medit-edit-in-minibuffer (&optional bounds)
  "Edit all matches of STR in BOUNDS."
  (medit-reset-vars)
  (let ((regexp (medit--regexp medit-string))
        (buffer (current-buffer))
        initial pos)
    (setq medit--bounds bounds)
    (medit--find-regexp-at-point regexp)
    (setq initial (match-string-no-properties 0))
    (setq pos (- (match-end 0) (point)))
    (move-overlay medit--edit-overlay
                  (match-beginning 0)
                  (match-end 0))
    (unwind-protect
        (minibuffer-with-setup-hook
            (:append
             (lambda ()
               (medit--setup-keymap)
               (add-hook 'after-change-functions
                         #'medit--minibuffer-after-change-function
                         nil t)
               (ignore-errors
                 (backward-char pos))))

	  (with-current-buffer buffer
            (medit--find-matches))
          (read-from-minibuffer
           (format "Replace `%s' with: " initial)
           initial))
      (medit-reset-vars))))

(defun medit-in-buffer-cancel ()
  "Exit medit session and undo the changes."
  (interactive)
  (medit-reset-vars)
  (when medit--in-buffer-modified
    (undo)))

(defvar-keymap medit-edit-map
  "M-RET" #'medit-replace
  "C-g" #'medit-in-buffer-cancel)

(defun medit-edit-in-buffer (&optional bounds)
  "Edit all matches of STR in BOUNDS."
  (undo-boundary)
  (medit-reset-vars)
  (setq medit--bounds bounds)
  (set (make-local-variable 'medit--in-buffer-modified) nil)
  (let ((regexp (medit--regexp medit-string)))
    (medit--find-regexp-at-point regexp)
    (move-overlay medit--edit-overlay
		  (match-beginning 0)
		  (match-end 0))
    (overlay-put medit--edit-overlay
		 'keymap medit-edit-map)
    (overlay-put medit--edit-overlay 'display nil)
    (medit--find-matches)))

(defun medit-edit (&optional bounds)
  (if medit-use-minibuffer
      (medit-edit-in-minibuffer bounds)
    (medit-edit-in-buffer bounds)))

;;;; Commands

(defun medit-replace ()
  "Apply the replacement."
  (interactive)
  (let ((replacement (minibuffer-contents-no-properties)))
    (dolist (match medit--matches)
      (with-current-buffer (overlay-buffer match)
	(goto-char (overlay-start match))
	(delete-region (overlay-start match) (overlay-end match))
	(insert replacement))))
  (when medit-use-minibuffer
    (exit-minibuffer)))

(defun medit-previous ()
  "Jump to the previous match."
  (interactive)
  (with-minibuffer-selected-window
    (goto-char (overlay-start medit--edit-overlay))
    (when (re-search-backward (medit--regexp medit-string) nil t)
      (move-overlay medit--edit-overlay
                    (match-beginning 0)
                    (match-end 0)))))

(defun medit-next ()
  "Jump to the next match."
  (interactive)
  (with-minibuffer-selected-window
    (goto-char (overlay-end medit--edit-overlay))
    (when (re-search-forward (medit--regexp medit-string) nil t)
      (move-overlay medit--edit-overlay
                    (match-beginning 0)
                    (match-end 0)))))

(defun medit-toggle-word ()
  "Toggle word style."
  (interactive)
  (pcase medit-style
    ('word (setq medit-style 'plain))
    (_  (setq medit-style 'word)))
  (with-minibuffer-selected-window
    (medit--find-matches))
  (message "Match style `%s'" medit-style))

(defun medit-toggle-symbol ()
  "Toggle symbol style."
  (interactive)
  (pcase medit-style
    ('symbol (setq medit-style 'plain))
    (_ (setq medit-style 'symbol)))
  (with-minibuffer-selected-window
    (medit--find-matches))
  (message "Match style `%s'" medit-style))

;;;###autoload
(defun medit-dwim (arg)
  "Main entry of medit.

If transient mark mode is enabled and the region is active, edit
the region.  Otherwise, edit the symbol at point.

If ARG or interactively the `prefix-arg' is non-nil, the edit is
limited to the current defun.

This function internally calls `medit-edit' to do the work."
  (interactive "P")
  (setq medit-style medit-default-style)
  (setq medit-string
        (if (use-region-p)
            (buffer-substring-no-properties
             (use-region-beginning) (use-region-end))
          (medit--string)))
  (medit-edit (and arg (bounds-of-thing-at-point 'defun))))


;; Isearch Integration

;;;###autoload
(defun medit-from-isearch ()
  "Convert an active isearch session to medit."
  (interactive)
  (require 'isearch)
  (let ((regexp (cond
                 ((functionp isearch-regexp-function)
                  (funcall isearch-regexp-function isearch-string))
                 (isearch-regexp-function
                  (word-search-regexp isearch-string))
                 (isearch-regexp isearch-string)
                 (t (regexp-quote isearch-string))))
        (str (buffer-substring-no-properties
              (overlay-start isearch-overlay)
              (overlay-end isearch-overlay))))
    (isearch-exit)
    (setq medit-string str)
    (setf (alist-get 'isearch medit-style-alist)
          (lambda (str)
            (if str regexp medit-string)))
    (setq medit-style 'isearch)
    (unwind-protect
        (medit-edit)
      (delq 'isearch medit-style-alist))))

;;;###autoload
(defalias 'isearch-to-medit #'medit-from-isearch)


(provide 'medit)
;;; medit.el ends here
