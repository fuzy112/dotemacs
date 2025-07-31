;;; linkding.el --- Linkding integration (WIP) -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.6
;; Keywords: hypermedia

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
;;; Code:

(require 'url-parse)
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))

(defgroup linkding nil
  "Linkding."
  :group 'net
  :prefix "linkding-")

(defcustom linkding-token ""
  "Linkding token."
  :type '(choice string function))

(defvar-local linkding--buffer-info nil)

(defun linkding--get-token ()
  (if (functionp linkding-token)
      (funcall linkding-token)
    linkding-token))

(cl-defstruct linkding-bookmark
  "Linkding bookmark."
  id url title description notes
  website-title website-description
  web-archive-snapshot-url is-archived
  unread shared tag-names
  date-added date-modified)

(defun linkding--get-auth-header ()
  `("Authorization" . ,(format "Token %s" (linkding--get-token))))

(defun linkding--parse-response ()
  (goto-char (point-max))
  (let ((status (thing-at-point 'number)))
    (if (null status) (error "Test: buf %S" (current-buffer)))
    (delete-region (line-beginning-position) (point-max))
    (setq-local linkding--buffer-info
                (plist-put linkding--buffer-info
                           :status status))
    (goto-char (point-min))
    (and (>= status 200) (< status 300)
         (json-parse-buffer :object-type 'alist
                            :array-type 'list
                            :null-object nil))))

(defun linkding--curl-make-sentinel (callback)
  (lambda (proc _message)
    (with-current-buffer (process-buffer proc)
      (let ((res (linkding--parse-response))
            (info linkding--buffer-info))
        (unless (process-live-p proc)
          (if callback
              (with-current-buffer (plist-get info :buffer)
                (funcall callback
                         res
                         info))
            (message "%S\n%S" res info)))))))

(cl-defun linkding--curl-call (url &key method headers data callback)
  (setf method (or method "GET"))
  (let* ((opts `("-s" "-X" ,method
                 "-w" "\n%{response_code}"
                 ,@(mapcan (lambda (h)
                             `("-H" ,(format "%s: %s" (car h) (cdr h))))
                           headers)
                 ,@(and data `("-d" ,data))
                 ,url))
         (buf (generate-new-buffer (format "*linkding-curl for %s*" url)))
         (proc (apply #'start-process "*linkding-curl*"
                      buf
                      "curl"
                      opts)))
    (let ((info  `( :status nil
                    :buffer ,(current-buffer)
                    :url ,url
                    :method ,method
                    :headers ,headers
                    :opts ,opts
                    :data ,data)))
      (with-current-buffer buf
        (setq-local linkding--buffer-info info)))
    (set-process-sentinel proc (linkding--curl-make-sentinel callback))))

(defcustom linkding-base-url ""
  "Base URL for linkding instance."
  :type 'string)

(defun linkding--call-api (endpoint &rest args)
  (declare (indent 1))
  (let ((url (concat linkding-base-url endpoint))
        (headers (list (linkding--get-auth-header))))
    (apply #'linkding--curl-call url :headers headers args)))

(cl-defun linkding--list-bookmarks (&key q limit offset callback)
  (let* ((query-string (url-build-query-string `(,(and q `(q ,q))
                                                 ,(and limit `(limit ,limit))
                                                 ,(and offset `(offset ,offset)))))
         (endpoint (concat "/api/bookmarks/?" query-string)))
    (linkding--call-api
     endpoint
     :callback
     (lambda (res info)
       (let (results count next previous)
	 (if res
	     (let-alist res
	       (setq count .count
		     next .next
		     previous .previous)
	       (dolist (b .results)
		 (let-alist b
		   (push (make-linkding-bookmark
			  :id .id
			  :url .url
			  :title .title
			  :description .description
			  :notes .notes
			  :website-title .website_title
			  :website-description .website_description
			  :web-archive-snapshot-url .web_archive_snapshot_url
			  :is-archived .is_archived
			  :unread .unread
			  :shared .shared
			  :tag-names .tag_names
			  :date-added .date_added
			  :date-modified .date_modified)
			 results)))
	       (setq results (nreverse results))))
	 (funcall callback
                  `( :count ,count
                     :next ,next
                     :previous ,previous
                     :results ,results)
                  info))))))

;;;###autoload
(define-derived-mode linkding-bookmark-list-mode special-mode
  "Linkding Bookmark List Mode."
  "A major mode for displaying Linkding bookmarks.")

(defun linkding--format-bookmark (bookmark)
  (apply #'insert-text-button
	 (linkding-bookmark-url bookmark)
         `(,@(and-let* ((title (linkding-bookmark-title bookmark))
                        ((not (string-empty-p title))))
               `(display ,title))))
  (newline)
  (insert "  ")
  (if (linkding-bookmark-is-archived bookmark)
      (insert-text-button (linkding-bookmark-web-archive-snapshot-url bookmark)
                          'display (format "%s ∞"
                                           (or (linkding-bookmark-date-modified bookmark)
                                               (linkding-bookmark-date-added bookmark))))
    (insert (or (linkding-bookmark-date-modified bookmark)
		(linkding-bookmark-date-added bookmark))))
  (insert " | ")
  (newline))

;;;###autoload
(defun linkding-list-bookmarks ()
  "List Linkding bookmarks in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*linkding-bookmarks*")
    (linkding-bookmark-list-mode)
    (linkding--list-bookmarks
     :callback
     (lambda (res _info)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (dolist (b (plist-get res :results))
           (linkding--format-bookmark b))
         (goto-char (point-min)))))
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defalias 'list-linkding-bookmarks #'linkding-list-bookmarks)

;;;###autoload
(defun linkding-to-emacs-bookmarks ()
  (interactive)
  (linkding--list-bookmarks
   :limit 1000
   :callback
   (lambda (res _info)
     (pcase-dolist ((cl-struct linkding-bookmark title url description notes
			       tag-names)
		    (plist-get res :results))
       (let ((record `(
		       (location . ,url)
		       (tag-names . ,tag-names)
		       (handler . ,#'url-bookmark-jump)
		       (annotation . ,(concat description "\n" notes))))
	     (name (if (string-empty-p title) url title)))
	 (if-let* ((existing (cl-find url bookmark-alist
				     :key (pcase-lambda (`(,_name . ,data)) (alist-get 'location data))
				     :test #'equal)))
	     (progn
	       (setcar existing name)
	       (setcdr existing record))
	   (bookmark-store name record t))))
     (bookmark-save))))

(provide 'linkding)
;;; linkding.el ends here

;; Local Variables:
;; jinx-local-words: "Linkding"
;; End:
