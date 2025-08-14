;;; consult-browser-hist.el --- Consult interface for browser history  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "29.1") (browser-hist "0.0.1") (consult "1.10"))

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(require 'browser-hist)
(require 'consult)

(defgroup consult-browser-hist ()
  "Consult searching browser history."
  :group 'consult
  :prefix "consult-browser-hist-")

(defface consult-browser-hist-title
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight item titles.")

(defface consult-browser-hist-url
  '((t :inherit font-lock-doc-face))
  "Face used to highlight item urls.")

(defface consult-browser-hist-type
  '((t :inherit font-lock-type-face))
  "Face used to highlight browser types.")

(defcustom consult-browser-hist-title-max-width .4
  "Maximum width of the entry titles displayed."
  :type '(choice (natnum :tag "Number of columns")
                 (float :tag "Ratio in the window")))

(defun consult-browser-hist--format (item)
  (let ((cand (concat (truncate-string-to-width
                       (cdr item)
                       (if (floatp consult-browser-hist-title-max-width)
                           (floor (* .4 (window-width (minibuffer-window))))
                         consult-browser-hist-title-max-width)
                       nil nil :truncate :ellipsis-text-property)
                      (propertize (concat "\t" (car item)) 'invisible t))))
    (add-text-properties 0 (length cand)
                         `( consult-browser-hist-url ,(car item)
                            consult-browser-hist-title ,(cdr item)
                            face consult-browser-hist-title)
                         cand)
    cand))

(defun consult-browser-hist--annotate (type cand)
  (let* ((url (get-text-property 0 'consult-browser-hist-url cand)))
    (consult--annotate-align
     cand
     (concat (propertize type 'face 'consult-browser-hist-type) "   "
             (propertize url 'face 'consult-browser-hist-url)))))

(defun consult-browser-hist--async (browser)
  (lambda (async)
    (let (connection)
      (lambda (action)
        (pcase action
          ('setup
           (browser-hist--make-db-copy browser))
          ('destroy
           (when connection
             (sqlite-close connection)
             (setq connection nil))))
        (let ((browser-hist-default-browser browser)
              (browser-hist--db-connection connection))
          (unwind-protect
              (funcall async action)
            (setq connection browser-hist--db-connection)))))))

(defun consult-browser-hist--send-query (input &optional callback)
  (let ((items (browser-hist--send-query input)))
    (when callback
      (funcall callback items)
      items)))

(defun consult-browser-hist--collection (browser)
  (consult--async-pipeline
   (consult--async-min-input)
   (consult--async-throttle)
   (consult-browser-hist--async browser)
   (consult--async-dynamic #'consult-browser-hist--send-query)
   (consult--async-map #'consult-browser-hist--format)
   (consult--async-highlight)))

(defun consult-browser-hist-source-make (name browser narrow-key &optional db-path db-fields)
  (when db-path
    (setf (alist-get browser browser-hist-db-paths) db-path))
  (when db-fields
    (setf (alist-get browser browser-hist--db-fields) db-fields))
  (list :name name
        :narrow narrow-key
        :category 'consult-browser-hist
        :action (lambda (selected)
                  (browse-url (get-text-property 0 'consult-browser-hist-url selected)))
        :enabled (lambda ()
                   (and-let* ((path (alist-get browser browser-hist-db-paths)))
                     (file-expand-wildcards (substitute-in-file-name path))))
        :annotate (apply-partially #'consult-browser-hist--annotate name)
        :async (consult-browser-hist--collection browser)))

(defvar consult-firefox-hist-source
  (consult-browser-hist-source-make "FireFox" 'firefox ?f))

(defvar consult-chromium-hist-source
  (consult-browser-hist-source-make "Chromium" 'chromium ?c))

(defvar consult-chrome-hist-source
  (consult-browser-hist-source-make "Chrome" 'chrome ?C))

(defvar consult-brave-hist-source
  (consult-browser-hist-source-make "Brave" 'brave ?b))

(defvar consult-qutebrowser-hist-source
  (consult-browser-hist-source-make "QuteBrowser" 'qutebrowser ?q))

(defvar consult-safari-hist-source
  (consult-browser-hist-source-make "Safari" 'safari ?s))

(defcustom consult-browser-hist-sources
  '(consult-firefox-hist-source
    consult-chromium-hist-source
    consult-chrome-hist-source
    consult-brave-hist-source
    consult-qutebrowser-hist-source
    consult-safari-hist-source)
  "Browser history sources to search."
  :type '(repeat symbol))

(defvar consult-browser-hist-history nil
  "Minibuffer history for `consult-browser-hist'.")

;;;###autoload
(defun consult-browser-hist (&optional sources)
  "Search through browser history."
  (interactive)
  (consult--multi
   (or sources  consult-browser-hist-sources)
   :prompt "Browser history: "
   :sort nil
   :require-match t
   :history 'consult-browser-hist-history))

(provide 'consult-browser-hist)
;;; consult-browser-hist.el ends here
