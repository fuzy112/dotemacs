;;; consult-omni-kagi.el --- Consult-Omni sources for Kagi -*- lexical-binding: t -*-
;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "29.1") (consult-omni "0.2"))
;; Version: 0.1.0
;; Keywords: convenience

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

;; consult-omni-kagi provides commands for searching Kagi in Emacs using
;; consult-omni.

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-kagi-api-key nil
  "Kagi API key."
  :type '(choice (string)
		 (function))
  :group 'consult-omni)

(defcustom consult-omni-kagi-enrich-web-api-url "https://kagi.com/api/v0/enrich/web"
  "API URL for Kagi Enrich Web."
  :type 'string
  :group 'consult-omni)

(defcustom consult-omni-kagi-search-url "https://kagi.com/search"
  "Web search URL for Kagi."
  :type 'string
  :group 'consult-omni)

(cl-defun consult-omni-kagi-enrich-web--request
    (input
     &rest args
     &key callback
     &allow-other-keys)
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
	       ;; (opts (car-safe opts))
	       (params `(("q" . ,query)))
	       (headers `(("User-Agent" . "Emacs:consult-omni/0.1 (Emacs consult-omni package;https://github.com/armindarvish/consult-omni)")
			  ("Accept" . "application/json")
			  ("Accept-Encoding" . "gzip")
			  ("Authorization" . ,(concat "Bot " (consult-omni-expand-variable-function consult-omni-kagi-api-key)) ))))
    (ignore opts)
    (consult-omni--fetch-url consult-omni-kagi-enrich-web-api-url consult-omni-http-retrieve-backend
			     :encoding 'utf-8
			     :params params
			     :headers headers
			     :parser #'consult-omni--json-parse-buffer
			     :callback
			     (lambda (attrs)
			       ;; (debug attrs)
			       (when-let* ((raw-results (map-elt attrs "data"))
					   (annotated-results
					    (mapcar (lambda (item)
						      (let*
							  ((source "Kagi Enrich Web")
							   (url (gethash "url" item))
							   (title (gethash "title" item))
							   (snippet (gethash "snippet" item))
							   (search-url (consult-omni--make-url-string consult-omni-kagi-search-url params))
							   (decorated (funcall consult-omni-default-format-candidate
									       :source source
									       :query query
									       :url url
									       :search-url search-url
									       :title title
									       :snippet snippet)))
							(propertize decorated
								    :source source
								    :title title
								    :url url
								    :search-url search-url
								    :query query
								    :snippet snippet)))
						    raw-results)))
				 (funcall callback annotated-results)
				 annotated-results)))))

(consult-omni-define-source "Kagi Enrich Web"
			    :narrow-char ?k
			    :type 'dynamic
			    :require-match nil
			    :face 'consult-omni-engine-title-face
			    :request #'consult-omni-kagi-enrich-web--request
			    :on-new (apply-partially #'consult-omni-external-search-with-engine "Kagi Enrich Web")
			    :preview-key consult-omni-preview-key
			    :search-hist 'consult-omni--search-history
			    :select-hist 'consult-omni--selection-history
			    :enabled (lambda () (bound-and-true-p consult-omni-kagi-api-key))
			    :group #'consult-omni--group-function
			    :sort t
			    :interactive consult-omni-intereactive-commands-type)

(provide 'consult-omni-kagi)

;;;###autoload
(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-kagi)
