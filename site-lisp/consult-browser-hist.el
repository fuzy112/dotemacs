;;; consult-browser-hist.el --- Consult interface for browser history  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "29.1") (browser-hist "0.0.1"))

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

(defgroup consult-browser-hist ()
  "Consult searching browser historyy"
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

(defun consult-browser-hist--transform (item)
  (let ((cand (concat (cdr item)
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

(defun consult-browser-hist--highlight (async)
  (consult--async-highlight
   async
   (lambda (input)
     (apply-partially #'consult--highlight-regexps
                      (mapcar #'regexp-quote (split-string input))
                      t))))

(defun consult-browser-hist--async (async browser)
  (let (connection)
    (lambda (action)
      (pcase action
        ('setup
         (browser-hist--make-db-copy browser))
        ('destroy
         (when connection
           (sqlite-close connection))))
      (let ((browser-hist-default-browser browser)
            (browser-hist--db-connection connection))
        (unwind-protect
            (funcall async action)
          (setq connection browser-hist--db-connection))))))

(defun consult-browser-hist--collection (async browser)
  (thread-first
    (consult-browser-hist--highlight async)
    (consult-browser-hist--async browser)
    (consult--async-map #'consult-browser-hist--transform)
    (consult--dynamic-compute #'browser-hist--send-query)
    (consult--async-throttle)))

(defun consult-browser-hist-source-make (name browser &optional db-path db-fields)
  (when db-path
    (setf (alist-get browser browser-hist-db-paths) db-path))
  (when db-fields
    (setf (alist-get browser browser-hist--db-fields) db-fields))
  `( :name ,name
     :narrow ?f
     :category consult-browser-hist
     :action ,(lambda (selected)
                (browse-url (get-text-property 0 'consult-browser-hist-url selected)))
     :enabled ,(lambda ()
                 (and-let* ((path (alist-get browser browser-hist-db-paths)))
                   (file-expand-wildcards (substitute-in-file-name path))))
     :annotate ,(apply-partially #'consult-browser-hist--annotate name)
     :async ,(lambda (async)
               (consult-browser-hist--collection async browser))))

(defvar consult-firefox-hist-source
  (consult-browser-hist-source-make "FireFox" 'firefox))

(defvar consult-chromium-hist-source
  (consult-browser-hist-source-make "Chromium" 'chromium))

(defvar consult-chrome-hist-source
  (consult-browser-hist-source-make "Chrome" 'chrome))

(defvar consult-brave-hist-source
  (consult-browser-hist-source-make "Brave" 'brave))

(defvar consult-qutebrowser-hist-source
  (consult-browser-hist-source-make "QuteBrowser" 'qutebrowser))

(defvar consult-safari-hist-source
  (consult-browser-hist-source-make "Safari" 'safari))

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
