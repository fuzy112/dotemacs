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

(require 'magit-commit)
(require 'magit-diff)
(require 'gptel-request)
(require 'gptel-transient)
(require 'transient)

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

(defcustom magit-gptel-max-tokens 4096
  "Maximum number of tokens for gptel requests from magit-gptel."
  :type 'integer
  :group 'magit-gptel)

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

(defface magit-gptel-thinking-heading
  '((t :inherit outline-1))
  "Face for the thinking heading.")

(defface magit-gptel-thinking-text
  '((t :inherit shadow))
  "Face for the thinking text.")

(defun magit-gptel--run-git (&rest args)
  (let ((status (apply #'magit-process-git t args)))
    (unless (zerop status)
      (let ((msg (format "git %s exited with %s" (string-join args " ") status)))
	(insert msg)
	(message "%s" msg)))))

(defun magit-gptel--commit-diff ()
  (pcase-let ((`(,rev ,arg ,_noalt) (magit-commit-diff--args)))
    (apply #'magit-gptel--run-git "diff" arg "-p" "-W"
	   (append
	    (car (magit-diff-arguments))
	    (list rev)))))

(defun magit-gptel--context (rationale)
  (let ((dir (magit-toplevel)))
    (with-current-buffer (get-buffer-create " *magit-gptel*")
      (setq-local default-directory dir)
      (erase-buffer)
      (insert "<git-command>")
      (insert (format "%s" (magit-repository-local-get 'last-commit-command)))
      (insert "</git-command>\n")
      (insert "<git-status>")
      (magit-gptel--run-git "status")
      (insert "</git-status>\n")
      (insert "<git-diff-staged>")
      (magit-gptel--commit-diff)
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

(defmacro magit-gptel--with-backend (&rest body)
  "Evaluate BODY with gptel backend and model bound to magit-gptel values.
If `magit-gptel-backend' is set, `gptel-backend' is bound to the matching
backend.  If `magit-gptel-model' is set, `gptel-model' is bound to that value."
  (declare (indent 0) (debug body))
  `(let ((gptel-backend gptel-backend)
	 (gptel-model gptel-model)
	 (gptel-include-reasoning 'ignore)
	 (gptel-use-tools nil)
	 (gptel-max-tokens magit-gptel-max-tokens))
     (when magit-gptel-backend
       (setq gptel-backend (if (stringp magit-gptel-backend)
			       (gptel-get-backend magit-gptel-backend)
			     magit-gptel-backend)))
     (when magit-gptel-model
       (setq gptel-model magit-gptel-model))
     ,@body))

(defun magit-gptel--stream-callback (response info)
  (let* ((gptel-buffer (plist-get info :buffer))
	 (start-marker (plist-get info :position))
	 (tracking-marker (plist-get info :tracking-marker))
	 (thinking-overlay (plist-get info :thinking-overlay))
	 move-point)
    (unless (markerp tracking-marker)
      (setq tracking-marker (copy-marker (or tracking-marker start-marker) t))
      (plist-put info :tracking-marker tracking-marker))
    (unless thinking-overlay
      (with-current-buffer (marker-buffer start-marker)
	(save-excursion
	  (goto-char tracking-marker)
	  (insert "\n")
	  (setq thinking-overlay (make-overlay start-marker (point)))
	  (plist-put info :thinking-overlay thinking-overlay)
	  (overlay-put thinking-overlay 'display (propertize "* thinking\n" 'face 'magit-gptel-thinking-heading))
	  (overlay-put thinking-overlay 'after-string "\n"))))
    (pcase-exhaustive response
      ((pred stringp)
       (with-current-buffer (marker-buffer start-marker)
	 (save-excursion
	   (goto-char tracking-marker)
	   (insert response))))
      ('abort
       (message "LLM query aborted"))
      ('nil
       (message "LLM query error: %s" (plist-get info :status)))
      ('t
       (message "LLM query finished"))
      (`(reasoning . ,(and text (pred stringp)))
       (with-current-buffer (marker-buffer start-marker)
	 (save-excursion
	   (goto-char tracking-marker)
	   (overlay-put thinking-overlay 'display
			(concat (overlay-get thinking-overlay 'display)
				(propertize text 'face 'magit-gptel-thinking-text))))))
      (`(reasoning . t)
       (when-let* ((buf (marker-buffer start-marker))
		   (win (get-buffer-window buf 0)))
	 (with-selected-window win
	   (goto-char tracking-marker)
	   (recenter 1)))))))

(defvar magit-gptel--flag nil)

(transient-define-infix magit-gptel:/g ()
  :description "Enable gptel commit message generation"
  :class 'transient-lisp-variable
  :variable 'magit-gptel--flag
  :reader (lambda (prompt initial-input _history)
	    (let* ((c (car (read-multiple-choice prompt
						 '((?y "yes")
						   (?n "no")
						   (?r "specify rationale"))))))
	      (if (eql c ?r)
		  (read-string prompt
			       (if (stringp initial-input)
				   initial-input))
		(eql c ?y))))
  :key "/g")

(transient-define-infix magit-gptel:/m ()
  :description "magit gptel model"
  :class 'gptel-provider-variable
  :prompt "Model: "
  :variable 'magit-gptel-model
  :set-value #'gptel--set-with-scope
  :backend 'magit-gptel-backend
  :key "/m"
  :reader (lambda (prompt &rest _)
	    (cl-loop
	     for (name . backend) in gptel--known-backends
	     nconc (cl-loop for model in (gptel-backend-models backend)
			    collect (list (concat name ":" (gptel--model-name model))
					  backend model))
	     into models-alist
	     with completion-extra-properties =
	     `(:annotation-function
	       ,(lambda (comp)
		  (let* ((model (nth 2 (assoc comp models-alist)))
			 (desc (get model :description))
			 (caps (get model :capabilities))
			 (context (get model :context-window))
			 (input-cost (get model :input-cost))
			 (output-cost (get model :output-cost))
			 (cutoff (get model :cutoff-date)))
		    (when (or desc caps context input-cost output-cost cutoff)
		      (concat
		       (propertize " " 'display `(space :align-to 40))
		       (when desc (truncate-string-to-width desc 70 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 112))
		       (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 134))
		       (when context (format "%5dk" context))
		       " " (propertize " " 'display `(space :align-to 142))
		       (when input-cost (format "$%5.2f in" input-cost))
		       (if (and input-cost output-cost) "," " ")
		       " " (propertize " " 'display `(space :align-to 153))
		       (when output-cost (format "$%6.2f out" output-cost))
		       " " (propertize " " 'display `(space :align-to 166))
		       cutoff)))))
	     finally return
	     (cdr (assoc (completing-read prompt models-alist nil t nil nil
					  (concat (gptel-backend-name magit-gptel-backend) ":"
						  (gptel--model-name magit-gptel-model)))
			 models-alist)))))

(unless (transient--locate-child 'magit-commit "/g")
  (transient-append-suffix 'magit-commit [0]
    [["gptel backend"
      (magit-gptel:/g)
      (magit-gptel:/m)]]))

(defun magit-gptel-generate-commit-message ()
  (when (and magit-gptel--flag
	     (eq (current-buffer) (magit-commit-message-buffer)))
    (kill-region (point-min) (point-max))
    (magit-gptel--with-backend
      (let* ((fsm (gptel-request
		      (magit-gptel--context
		       (and (stringp magit-gptel--flag) magit-gptel--flag))
		    :system magit-gptel-system-message
		    :stream t
		    :callback #'magit-gptel--stream-callback))
	     (query-fun (lambda (force)
			  (when force
			    (gptel-abort (current-buffer)))
			  (pcase (gptel-fsm-state fsm)
			    ('DONE
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
			   (gptel-abort (current-buffer))
			   (erase-buffer))))
	(add-hook 'with-editor-finish-query-functions query-fun nil t)
	(add-hook 'with-editor-pre-cancel-hook pre-cancel nil t))
      (message "Querying %s:%s..."
	       (gptel-backend-name gptel-backend)
	       gptel-model))
    (setq magit-gptel--flag nil)))

(add-hook 'with-editor-filter-visit-hook #'magit-gptel-generate-commit-message 10)
(add-hook 'server-switch-hook #'magit-gptel-generate-commit-message 10)

(provide 'magit-gptel)
;;; magit-gptel.el ends here
