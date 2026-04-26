;;; consult-browser-hist.el --- Consult interface for browser history  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Zhengyi Fu

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

(defface cbh-title
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight item titles.")

(defface cbh-url
  '((t :inherit font-lock-doc-face))
  "Face used to highlight item urls.")

(defface cbh-type
  '((t :inherit font-lock-type-face))
  "Face used to highlight browser types.")

(defcustom cbh-title-max-width .4
  "Maximum width of the entry titles displayed."
  :type '(choice (natnum :tag "Number of columns")
                 (float :tag "Ratio in the window")))

(defun cbh--format (item)
  (let ((cand (concat (truncate-string-to-width
                       (cdr item)
                       (if (floatp cbh-title-max-width)
                           (floor (* .4 (window-width (minibuffer-window))))
                         cbh-title-max-width)
                       nil nil :truncate :ellipsis-text-property)
                      (propertize (concat "\t" (car item)) 'invisible t))))
    (add-text-properties 0 (length cand)
                         `( cbh-url ,(car item)
                            cbh-title ,(cdr item)
                            face cbh-title)
                         cand)
    cand))

(defun cbh--annotate (type cand)
  (let* ((url (get-text-property 0 'cbh-url cand)))
    (consult--annotate-align
     cand
     (concat (propertize type 'face 'cbh-type) "   "
             (propertize url 'face 'cbh-url)))))

(defun cbh--async (browser)
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

(defun cbh--send-query (input &optional callback)
  (let ((items (browser-hist--send-query input)))
    (when callback
      (funcall callback items)
      items)))

(defun cbh--collection (browser)
  (consult--async-pipeline
   (consult--async-min-input)
   (consult--async-throttle)
   (cbh--async browser)
   (consult--async-dynamic #'cbh--send-query)
   (consult--async-map #'cbh--format)
   (consult--async-highlight)))

(defun cbh-source-make (name browser narrow-key &optional db-path db-fields)
  (when db-path
    (setf (alist-get browser browser-hist-db-paths) db-path))
  (when db-fields
    (setf (alist-get browser browser-hist--db-fields) db-fields))
  (list :name name
        :narrow narrow-key
        :category 'consult-browser-hist
        :action (lambda (selected)
                  (browse-url (get-text-property 0 'cbh-url selected)))
        :enabled (lambda ()
                   (and-let* ((path (alist-get browser browser-hist-db-paths)))
                     (file-expand-wildcards (substitute-in-file-name path))))
        :annotate (apply-partially #'cbh--annotate name)
        :async (cbh--collection browser)))

(defvar cbh-source-firefox
  (cbh-source-make "FireFox" 'firefox ?f))

(defvar cbh-source-chromium
  (cbh-source-make "Chromium" 'chromium ?c))

(defvar cbh-source-chrome
  (cbh-source-make "Chrome" 'chrome ?C))

(defvar cbh-source-brave
  (cbh-source-make "Brave" 'brave ?b))

(defvar cbh-source-qutebrowser
  (cbh-source-make "QuteBrowser" 'qutebrowser ?q))

(defvar cbh-source-safari
  (cbh-source-make "Safari" 'safari ?s))

(defcustom cbh-sources
  '(cbh-source-firefox
    cbh-source-chromium
    cbh-source-chrome
    cbh-source-brave
    cbh-source-qutebrowser
    cbh-source-safari)
  "Browser history sources to search."
  :type '(repeat symbol))

(defvar cbh-history nil
  "Minibuffer history for `consult-browser-hist'.")

;;;###autoload
(defun consult-browser-hist (&optional sources)
  "Search through browser history."
  (interactive)
  (consult--multi
   (or sources cbh-sources)
   :prompt "Browser history: "
   :sort nil
   :require-match t
   :history 'cbh-history))

(provide 'consult-browser-hist)

;; Local Variables:
;; read-symbol-shorthands: (("cbh-" . "consult-browser-hist-"))
;; End:

;;; consult-browser-hist.el ends here
