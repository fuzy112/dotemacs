;;; dotemacs-help.el  -*- lexical-binding: t; -*-

;; Copyright © 2024-2026  Zhengyi Fu <i@fuzy.me>

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


;;;; help

;; Enable editing of variable values in help buffers
(setq help-enable-variable-value-editing t
      help-enable-completion-autoload nil
      help-window-select t
      help-window-keep-selected t)

;; Add shortdoc examples to function help if available
(when (fboundp 'shortdoc-help-fns-examples-function)
  (add-hook 'help-fns-describe-function-functions
            #'shortdoc-help-fns-examples-function 50))

;; Insert the source code of the function in `describe-function' buffers.
(add-to-list 'help-fns-describe-function-functions #'help-fns-function-source-code 'append)

(defun help-fns-function-source-code (function)
  "Insert the Emacs Lisp source code for FUNCTION into the current buffer.
FUNCTION should be a symbol naming a function.  The source code is
extracted from the function's definition and inserted with proper
font-locking and indentation."
  (condition-case nil
      (pcase-let* ((saved-points nil)
                   (`(,buffer . ,point)
                    (let ((inhibit-interaction t)
                          (find-file-hook
                           (cons (lambda ()
                                   (push (cons (current-buffer) (point)) saved-points))
                                 find-file-hook)))
                      (save-window-excursion
                        (find-function-noselect function))))
                   (text (with-current-buffer buffer
                           (save-excursion
                             (goto-char point)
                             (end-of-defun)
                             (let ((end (point)))
                               (beginning-of-defun)
                               (font-lock-ensure (point) end)
                               (buffer-substring (point) end))))))
        (add-text-properties 0 (length text)
                             '(line-prefix (space :align-to 2))
                             text)
        (insert "\n  Source code:\n\n")
        (insert text)
        (insert "\n\n"))
    (error nil)))

;;;###autoload
(defun +mail-to-help-gnu-emacs ()
  (interactive)
  (compose-mail
   "help-gnu-emacs@gnu.org"
   (read-string "Title: ")))


(defvar url-http-response-status)
(declare-function mm-url-encode-multipart-form-data "mm-url.el")
(declare-function gnus-group-read-ephemeral-group "gnus-group.el")
(declare-function mml-compute-boundary "mml.el")
(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)
(defun +gnus-read-ephemeral-public-inbox-search-group (url query include-all)
  (interactive "sURL: \nsQuery: \nP")
  (require 'gnus-group)
  (require 'mm-url)
  (let* ((url (format "%s/?q=%s&x=m" url (url-hexify-string query)))
         (mbox (make-temp-file "mbox-"))
         (boundary (mml-compute-boundary '()))
         (values (list (if include-all
                           (cons "z" "full threads")
                         (cons "x" "results only"))))
         (url-request-method "POST")
         (url-request-extra-headers
          (list (cons "Content-Type"
                      (concat "multipart/form-data; boundary=" boundary))))
         (url-request-data
          (mm-url-encode-multipart-form-data values boundary)))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte nil)
      (goto-char (point-min))
      (forward-paragraph)
      (forward-line)
      (delete-region (point-min) (point))
      (if (>= url-http-response-status 400)
          (error "HTTP error: %s" (buffer-string)))
      (call-process-region nil nil "gzip" t t nil "-d")
      (write-region nil nil mbox))
    (gnus-group-read-ephemeral-group "emacs"
                                     `(nndoc ,query
                                             (nndoc-address ,mbox)))))

;;;###autoload
(defun +gnus-read-ephemeral-emacs-search-group (query include-all)
  (interactive "sQuery: \nP")
  (+gnus-read-ephemeral-public-inbox-search-group
   "https://yhetil.org/emacs"
   query include-all))

(provide 'dotemacs-help)
;;; dotemacs-help.el ends here
