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

(defcustom magit-gptel-use-streaming t
  "Whether to use streaming for gptel commit message generation."
  :type 'boolean)

(defmacro magit-gptel--with-backend (&rest body)
  "Evaluate BODY with gptel backend and model bound to magit-gptel values.
If `magit-gptel-backend' is set, `gptel-backend' is bound to the matching
backend.  If `magit-gptel-model' is set, `gptel-model' is bound to that value."
  (declare (indent 0) (debug body))
  `(let ((gptel-backend gptel-backend)
	 (gptel-model gptel-model)
	 (gptel-include-reasoning 'ignore)
	 (gptel-use-tools nil))
     (when magit-gptel-backend
       (setq gptel-backend (if (stringp magit-gptel-backend)
			       (gptel-get-backend magit-gptel-backend)
			     magit-gptel-backend)))
     (when magit-gptel-model
       (setq gptel-model magit-gptel-model))
     ,@body))


;;;###autoload
(defun magit-gptel-commit (rationale &rest args)
  "Create a git commit with a message generated by gptel.

RATIONALE is an optional explanatory text for the commit.  ARGS are
additional arguments passed to `magit-commit-create' or `git commit'.

When `magit-gptel-use-streaming' is non-nil, the commit is created
interactively after gptel streams its response into the commit message
buffer.  Otherwise the message is generated and the commit is made
non-interactively with `magit-run-git-with-editor'."
  (interactive (list nil (magit-commit-arguments)))
  (if magit-gptel-use-streaming
      (letrec ((hook 'git-commit-setup-hook)
	       (fun
		(lambda ()
		  (remove-hook hook fun)
		  (kill-region (point-min) (point-max))
		  (magit-gptel--with-backend
		    (let* ((fsm (gptel-request
				    (magit-gptel--context rationale args)
				  :system magit-gptel-system-message
				  :stream t))
			   (query-fun (lambda (force)
					(when force
					  (gptel-abort (current-buffer)))
					(pcase (gptel-fsm-state fsm)
					  ('DONE
					   (goto-char (point-min))
					   (while-let ((match (text-property-search-forward 'gptel 'ignore t)))
					     (delete-region (prop-match-beginning match)
							    (prop-match-end match)))
					   t)
					  ('ERRS
					   (message "gptel request failed")
					   (not force))
					  ('ABRT
					   t)
					  (_
					   (message "gptel request has not finished")
					   nil))))
			   (pre-cancel (lambda ()
					 (gptel-abort (current-buffer)))))
		      (add-hook 'with-editor-finish-query-functions query-fun nil t)
		      (add-hook 'with-editor-pre-cancel-hook pre-cancel nil t))
		    (message "Querying %s:%s..."
			     (gptel-backend-name gptel-backend)
			     gptel-model )))))
	(add-hook hook fun 10)
	(magit-commit-create))
    (magit-gptel--with-backend
      (let ((topdir (magit-toplevel))
	    (context (magit-gptel--context rationale args)))
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
		       (plist-get info :status))))))
      (message "Querying %s:%s..."
	       (gptel-backend-name gptel-backend)
	       gptel-model))))

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
