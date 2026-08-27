;;; magit-gptel.el --- Gptel integration for Magit   -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: vc, convenience

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

(require 'magit)
(require 'gptel)

(defgroup magit-gptel nil
  "Gptel integration for Magit."
  :group 'gptel
  :group 'magit
  :prefix "magit-gptel-")

;;;###autoload (put 'magit-gptel-backend 'safe-local-variable #'always)
;;;###autoload (put 'magit-gptel-model 'safe-local-variable #'always)

(defcustom magit-gptel-backend nil
  "Gptel backend used for magit-gptel.
See `gptel-backend'."
  :safe #'always
  :type (custom-variable-type 'gptel-backend)
  :set (lambda (sym val &optional local)
	 (let ((setter (get 'gptel-backend 'custom-set)))
	   (if local
	       (cl-letf (((symbol-function 'set-default-toplevel-value) #'set-local))
		 (funcall setter sym val))
	     (funcall setter sym val))))
  :get (get 'gptel-backend 'custom-get))

(defcustom magit-gptel-model nil
  "Gptel model used for magit-gptel.
See `gptel-model'."
  :safe #'always
  :type (custom-variable-type 'gptel-model))

(defcustom magit-gptel-system-message
  "You are a large language model and an experienced software developer.
You will receive diffs and optional context from Magit, Emacs' Git interface.
Your task is to generate a clean, concise yet informative commit message for the changes.
Focus exclusively on staged changes; omit any mention of unstaged or untracked changes.
The first line of the commit message should not excess 72 characters.
If previous Git commit history is provided, align your message with the existing project conventions:
- Follow existing formatting, such as bullet points for multiple changes
- Include descriptions of purpose and implementation details if that is the existing convention
Output only the commit message, with no extra explanation or surrounding markup.

If you find anything is wrong or unclear, stop immediately without outputing any commit message."
  "System message for magit-gptel-commit."
  :type 'string)

(defun magit-gptel--run-git (&rest args)
  (let ((status (apply #'magit-process-git t args)))
    (unless (zerop status)
      (error "git %s exited with %s" (string-join args " ") status))))

(defun magit-gptel--context (rationale &optional args)
  (let ((dir (magit-toplevel)))
    (with-current-buffer (get-buffer-create " *magit-gptel*")
      (setq-local default-directory dir)
      (erase-buffer)
      (insert "<git-status>")
      (magit-gptel--run-git "status")
      (insert "</git-status>\n")
      (insert "<git-diff-staged>")
      (let ((commit (if (member "--amend" args)
			"HEAD~"
		      "HEAD")))
	(magit-gptel--run-git "diff" "--cached" "--no-textconv" commit))
      (insert "</git-diff-staged>\n")
      (insert "<recent-commits>")
      (magit-gptel--run-git "log" "-n10" "--stat")
      (insert "</recent-commits>\n")
      (insert "<more-logs>")
      (magit-gptel--run-git "log" "-n30" "--oneline")
      (insert "</more-logs>\n")
      (when rationale
	(insert "<user-specified-rationale>")
	(insert rationale)
	(insert "</user-specified-rationale>\n"))
      (when-let* ((file (expand-file-name "info/commit-guide" (magit-gitdir)))
		  ((file-readable-p file)))
	(insert "<project-local-instructions>")
	(insert-file-contents file)
	(insert "</project-local-instructions>\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun magit-gptel-commit (rationale &rest args)
  (interactive (list nil (magit-commit-arguments)))
  (let ((gptel-backend gptel-backend)
	(gptel-model gptel-model)
	(topdir (magit-toplevel))
	(context (magit-gptel--context rationale)))
    (when magit-gptel-backend
      (setq gptel-backend (if (stringp magit-gptel-backend)
			      (gptel-get-backend magit-gptel-backend)
			    magit-gptel-backend)))
    (when magit-gptel-model
      (setq gptel-model magit-gptel-model))
    (gptel-request
	context
      :system magit-gptel-system-message
      :callback
      (lambda (response info)
	(if (stringp response)
	    (let ((default-directory topdir))
	      (apply #'magit-run-git-with-editor "commit" "--edit"
		     "-m" response args))
	  (message "gptel-request failed with message: %s"
		   (plist-get info :status)))))
    (message "Querying %s" (gptel-backend-name gptel-backend))))

;;;###autoload
(defun magit-gptel-commit-with-rationale (&optional args)
  (interactive (list (magit-commit-arguments)))
  (magit-gptel-commit (read-string "Rationale: ") args))

;;;###autoload
(with-eval-after-load 'magit-commit
  (transient-append-suffix 'magit-commit "c"
    '("g" "Generate commit" magit-gptel-commit))
  (transient-append-suffix 'magit-commit "g"
    '("r" "Commit with rationale" magit-gptel-commit-with-rationale)))

(provide 'magit-gptel)
;;; magit-gptel.el ends here
