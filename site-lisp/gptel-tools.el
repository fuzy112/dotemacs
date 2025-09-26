;;; gptel-tools.el --- Agentic tools for gptel  -*- lexical-binding: t -*-
;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

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

;; File operation tools for gptel.

;;; Code:

(require 'gptel)
(require 'with-editor)
(require 'flymake)
(eval-when-compile
  (require 'seq)
  (require 'llama))

;;;; UI and Popup Functions

;; User interface utilities for gptel-tools

(defun gptel-tools--popup-1 (prompt actions)
  "Display PROMPT and allow user to choose between one of several ACTIONS.
ACTIONS is a sequence of lists (KEY DESC FUNC ARGS...).  KEY is a string
identifying the key that triggers this action; it is passed to
`key-parse'.  DESC is a description string to be displayed in the popup.
If it is nil, the action and its binding is not displayed in the popup,
although it still takes effect.  If the user selects an action, its FUNC
is called with ARGS and popup is dismissed.  The return value of
`gptel-tools--popup-1' is the return value of FUNC."
  (let ((keymap (make-sparse-keymap))
	(prompt (concat prompt "\n"))
	(max-key-lenght (seq-reduce
			 (##max %1 (length (car %2)))
			 actions 0))
	func)
    (seq-doseq (action actions)
      (pcase-let ((`(,key ,desc ,func . ,args) action))
	(when desc
	  (setq prompt
		(concat prompt
			(format "\n %s%s  %s"
				(propertize key 'face 'help-key-binding)
				(make-string (- max-key-lenght (length key)) ?\s)
				desc))))
	(keymap-set keymap key (lambda ()
				 (interactive)
				 (apply func args)))))
    (setq prompt (concat prompt "\n\n"))
    (let ((max-mini-window-height 1.0)
	  (cursor-in-echo-area t))
      (when minibuffer-auto-raise
	(raise-frame (window-frame (minibuffer-window))))
      (while (null func)
	(setq func (keymap-lookup keymap
				  (key-description
				   (vector (read-key prompt)))))))
    (funcall func)))

(defmacro gptel-tools--popup (prompt actions)
  "Create a popup with PROMPT and ACTIONS.
ACTIONS is a sequence of (KEY DESC BODY...) forms, where KEY is the
keybinding, DESC is the description, and BODY is the code to execute
when KEY is selected.

See also `gptel-tools--popup-1'."
  `(gptel-tools--popup-1
    ,prompt
    (list ,@(mapcar
	     (lambda (action)
	       (pcase-let ((`(,key ,desc . ,body) action))
		 `(list ,key ,desc (lambda () ,@body))))
	     actions))))

(defvar ediff-window-setup-function)

;;;; Ediff Integration

;; Functions for integrating with ediff for file comparison and editing
(declare-function ediff-setup-windows-plain "ediff-wind.el" (arg1 arg2 arg3 arg4))

(defun gptel-tools--ediff-buffers (old-buffer new-buffer &optional startup-hooks)
  "Launch ediff between OLD-BUFFER and NEW-BUFFER with proper cleanup.

Ediff will be configured with plain window setup and will clean up the
NEW-BUFFER when finished.  The original window configuration will be
restored after ediff completes.

STARTUP-HOOKS are additional hooks to run after ediff setup."
  (let ((orig-window (selected-window))
	(orig-window-config (current-window-configuration))
	(ediff-window-setup-function #'ediff-setup-windows-plain)
	(mode (with-current-buffer old-buffer major-mode)))
    ;; make sure the current window is not a not side window
    (when (window-parameter orig-window 'window-side)
      (select-window (get-window-with-predicate
		      (lambda (win)
			(null (window-parameter win 'window-side))))))
    (with-current-buffer new-buffer
      (unless (derived-mode-p mode)
	(funcall mode)))
    (ediff-buffers
     old-buffer new-buffer
     (cons (lambda ()
	     (add-hook 'ediff-quit-hook
		       (lambda ()
			 (set-window-configuration orig-window-config))
		       90 t)
	     (add-hook 'ediff-quit-hook
		       (lambda ()
			 (kill-buffer new-buffer))
		       95 t))
	   startup-hooks))))


(declare-function ediff-quit "ediff-util.el" (arg1))

(defun gptel-tools--ediff-file-with-buffer (filename buffer callback &optional startup-hooks)
  "Launch ediff between FILENAME and BUFFER for reviewing changes.

CALLBACK is called with result message after user decision.
Optional STARTUP-HOOKS run after ediff setup.

Presents UI for accepting, rejecting, or requesting changes to the edit."
  (let* ((session-buffer (current-buffer))
	 (file-buffer (find-file-noselect filename))
	 (complete-fn (lambda (result)
			(when callback
			  (funcall callback result)
			  (setq callback nil))))
	 (accept-fn (lambda (&optional quit)
		      (interactive (list t))
		      (with-current-buffer buffer
			(write-region nil nil filename))
		      (funcall complete-fn "Successfully edited file.")
		      (when quit
			(ediff-quit nil))))
	 (quit-fn (lambda (&optional quit)
		    (interactive (list t))
		    (with-current-buffer file-buffer
		      (save-buffer))
		    (funcall complete-fn "User manually edited the file.  Check the file again to see what has changed.")
		    (when quit
		      (ediff-quit nil))))
	 (reject-fn (lambda (&optional quit)
		      (interactive (list t))
		      (funcall complete-fn "User rejected the changes.")
		      (gptel-abort session-buffer)
		      (when quit
			(ediff-quit nil))))
	 (iterate-fn (lambda (&optional quit)
		       (interactive (list t))
		       (funcall complete-fn
				(concat "User rejected the changes.  \
The changes have not been applied.  The user request you to do the \
following:\n"
					(read-string "Requested changes: ")))
		       (when quit
			 (ediff-quit nil))))
	 (quit-func (lambda ()
		      (when callback
			(gptel-tools--popup-1
			 "Accept changes? "
			 `(("a" "Apply all proposed changes" ,accept-fn)
			   ("q" "Quit and keep the file as is" ,quit-fn)
			   ("k" "Reject the changes" ,reject-fn)
			   ("r" "Iterate the changes" ,iterate-fn))))))
	 (kill-buffer-warning-fn (lambda ()
				   (interactive)
				   (message "Do not kill this buffer.
Use C-c C-a to accept the edit, C-c C-k to reject.")))
	 (startup-func (lambda ()
			 (let ((keymap (make-sparse-keymap)))
			   (set-keymap-parent keymap (current-local-map))
			   (keymap-set keymap "C-c C-a" accept-fn)
			   (keymap-set keymap "C-c C-k" reject-fn)
			   (keymap-set keymap "C-c C-c" quit-fn)
			   (keymap-set keymap "C-c C-r" iterate-fn)
			   (keymap-set keymap "C-x k" kill-buffer-warning-fn)
			   (keymap-set keymap "<remap> <kill-buffer>" kill-buffer-warning-fn)
			   (use-local-map keymap))
			 (add-hook 'ediff-quit-hook quit-func nil t))))
    (gptel-tools--ediff-buffers file-buffer
				buffer
				(cons startup-func startup-hooks))))


(defun gptel-tools--read-file (filepath start end)
  "Read the contents of a file from the file system.

FILEPATH is the path to the file to read.  Supports relative paths and ~.
START is the first line to read (Start from 1).
END is the last line of the file to read.  Can be -1 to read to end of file.

Returns the file contents as a string."
  (unless (file-exists-p filepath)
    (error "File not found: %s" filepath))
  (let ((file-buffer (find-file-noselect (expand-file-name filepath))))
    (with-current-buffer file-buffer
      (save-excursion
	(goto-char (point-min))
	(when (natnump start)
	  (forward-line (1- start)))
	(setq start (point))
	(if (or (null end) (< end 1))
	    (setq end (point-max))
	  (goto-char (point-min))
	  (forward-line (1- end))
	  (setq end (point))))
      (when (get-buffer-window file-buffer)
	(pulse-momentary-highlight-region start end))
      (buffer-substring-no-properties start end))))

(defun gptel-tools--read-file-confirm-p (filepath &optional _start _end)
  "Return non-nil if FILEPATH lies outside the current `default-directory'.
START and END are ignored."
  (not
   (string-prefix-p
    (expand-file-name default-directory)
    (expand-file-name filepath))))

(defun gptel-tools--list-directory (directory)
  "List DIRECTORY's contents."
  (mapconcat #'identity
	     (directory-files (expand-file-name directory))
	     "\n"))

(defun gptel-tools--create-file (path filename content)
  "Create file FILENAME under PATH with CONTENT."
  (let ((full-path (expand-file-name
		    filename (expand-file-name path))))
    (if (file-exists-p full-path)
	(error "File %s already exists" filename)
      (with-temp-buffer
	(insert content)
	(write-file full-path))
      (format "Created file %s in %s" filename path))))

(defun gptel-tools--write-file-async (callback file-path content)
  "Write CONTENT to FILE-PATH after user preview.
CALLBACK is called with a result string."
  (let* ((preview-buffer (generate-new-buffer "*write-file-preview*")))
    (condition-case err
	(with-current-buffer preview-buffer
	  (insert content))
      (:success (gptel-tools--ediff-file-with-buffer
		 (expand-file-name file-path) preview-buffer callback))
      (error (funcall callback (list :error "An error occurred: %S"
				     :internal-error (gptel--to-string err)))))))

(defun gptel-tools--insert-async (callback file-path insert-line new-string)
  "Insert NEW-STRING at line INSERT-LINE in FILE-PATH, then run CALLBACK.
CALLBACK is called with no arguments on success."
  (cl-assert (stringp file-path))
  (cl-assert (natnump insert-line))
  (cl-assert (stringp new-string))
  (let ((edit-buffer (generate-new-buffer "*edit-file*")))
    (condition-case-unless-debug err
	(with-current-buffer edit-buffer
	  (insert-file-contents (expand-file-name file-path))
	  (goto-char (point-min))
	  (unless (zerop (forward-line insert-line))
	    (error "Line number out of range"))
	  (insert new-string ?\n))
      (:success (gptel-tools--ediff-file-with-buffer
		 (expand-file-name file-path) edit-buffer callback))
      (error (funcall callback (list :error "An error occurred: %S"
				     :internal-error (gptel--to-string err)))))))

(defun gptel-tools--string-replace-async (callback file-path old-string new-string)
  "Replace OLD-STRING with NEW-STRING in FILE-PATH, confirming via ediff.
CALLBACK receives success or error message.
Errors if OLD-STRING is empty, missing, or not unique."
  (cl-assert (stringp file-path))
  (cl-assert (not (string-empty-p old-string)))
  (let ((edit-buffer (generate-new-buffer "*edit-file*")))
    (condition-case-unless-debug err
	(with-current-buffer edit-buffer
	  (insert-file-contents (expand-file-name file-path))
	  (let* ((inhibit-read-only t)
		 (case-fold-search nil)
		 (ediff-window-setup-function 'ediff-setup-windows-plain)
		 line-number)
	    (goto-char (point-min))
	    (unless (search-forward old-string nil t)
	      (error "Failed to find the string to replace"))
	    (setq line-number (line-number-at-pos))
	    (save-match-data
	      (when (search-forward old-string nil t)
		(error "Failed to replace: the `old_string' is not unique:
 - first occurrence: line %d\n
 - second occurrence: line %d\n"
		       line-number (line-number-at-pos))))
	    (replace-match new-string t t)))
      (:success (gptel-tools--ediff-file-with-buffer
		 (expand-file-name file-path) edit-buffer callback))
      (error (funcall callback (format "An error occurred: %S" err))
	     (kill-buffer edit-buffer)))))

(defun gptel-tools--grep-async (callback regexp working-dir)
  "Call CALLBACK with ripgrep output for REGEXP in WORKING-DIR."
  (condition-case err
      (let* ((default-directory (or (and working-dir (expand-file-name working-dir))
				    (expand-file-name default-directory)))
	     (compilation-buffer-name-function #'project-prefixed-buffer-name)
	     (buffer (grep (format "rg --sort=modified --no-heading -n -e %s | head -n 200"
				   (shell-quote-argument regexp)))))
	(with-current-buffer buffer
	  (add-hook 'compilation-finish-functions
		    (lambda (buffer _how)
		      (with-current-buffer buffer
			(goto-char (point-min))
			(forward-line 2)
			(let ((start (point-marker)))
			  (goto-char (point-max))
			  (forward-line -2)
			  (funcall callback (buffer-substring-no-properties
					     start (point)))
			  (goto-char (point-min)))))
		    nil t)))
    (error (funcall callback (list :error "Failed to run grep"
				   :internal-error (gptel--to-string err))))))

(defun gptel-tools--shell-command-async (callback command working-dir)
  "Run COMMAND in WORKING-DIR and call CALLBACK with its output string."
  (let* ((dir (if working-dir
		  (expand-file-name working-dir)
		(expand-file-name default-directory)))
	 (command-buffer-name (if (project-current)
				  (concat " *" (project-name (project-current)) " : Command Output*")
				" *Command Output*"))
	 (buffer (get-buffer-create command-buffer-name))
	 proc start-marker)
    (with-current-buffer buffer
      (setq default-directory dir)
      (unless (derived-mode-p 'comint-mode)
	(comint-mode)
	(setq-local process-environment (append (list "PAGER=cat"
						      "GIT_PAGER=cat")
						process-environment)))
      (goto-char (point-max))
      ;; Insert separator and command before running
      (insert "\n" (make-string 60 ?-) "\n")
      (insert-before-markers (propertize (format "%s $ " (abbreviate-file-name dir))
					 'face 'comint-highlight-prompt)
			     command "\n")
      (setq start-marker (point-marker)))
    (display-buffer buffer)
    (with-editor
      (comint-exec buffer
		   "shell-command"
		   (or explicit-shell-file-name shell-file-name)
		   nil
		   (list  "-c"
			  command)))
    (setq proc (get-buffer-process buffer))
    (set-process-sentinel
     proc
     (lambda (p _m)
       (unless (process-live-p p)
	 (funcall callback
		  (and (buffer-live-p buffer)
		       (with-current-buffer buffer
			 (buffer-substring-no-properties
			  start-marker (point-max))))))))))


(defvar gptel-tools-allowed-commands
  '("ls" "find" "date"))

(defvar gptel-tools-allowed-subcommands
  '(("git" "log" "diff" "show" "grep" "status")
    ("apt" "search" "list")))

(defun gptel-tools--shell-command-allowed-p (command)
  "Return non-nil if COMMAND is allowed to be executed by gptel tools.
COMMAND is considered allowed if it is listed in `gptel-tools-allowed-commands'
or if its first subcommand (ignoring leading options) is listed in
`gptel-tools-allowed-subcommands' for the corresponding main command."
  (let ((splitted (split-string-shell-command command))
	subcommand-entry)
    (cond ((member (car splitted) gptel-tools-allowed-commands)
	   t)
	  ((setq subcommand-entry (assoc (car splitted) gptel-tools-allowed-subcommands))
	   (let ((subcommand (progn (pop splitted)
				    (pop splitted))))
	     (while (and subcommand (string-prefix-p "-" subcommand))
	       (setq subcommand (pop splitted)))
	     (member subcommand (cdr subcommand-entry)))))))

(defun gptel-tools--shell-command-confirm-p (command working-dir)
  "Return non-nil if COMMAND should be confirmed in WORKING-DIR.
This is the case when either COMMAND is not allowed, or WORKING-DIR
falls outside the `default-directory' hierarchy."
  (not
   (and-let* ((allowed (gptel-tools--shell-command-allowed-p command))
	      (dir (expand-file-name (file-name-as-directory working-dir))))
     (string-prefix-p
      (expand-file-name default-directory)
      dir))))


(defun gptel-tools--flymake-diag-to-json (diag)
  "Convert a Flymake diagnostic DIAG to a JSON-compatible plist."
  (pcase-let (((cl-struct flymake--diag locus beg end type origin code message backend
			  data overlay-properties overlay)
	       diag))
    (ignore data overlay-properties overlay)
    (and (bufferp locus)
	 (with-current-buffer locus
	   (when (buffer-file-name)
	     (setq locus (buffer-file-name)
		   beg (line-number-at-pos beg)
		   end (line-number-at-pos end)))))
    (list :locus locus
	  :beg beg
	  :end end
	  :type type
	  :origin origin
	  :code code
	  :message message
	  :backend backend)))

(defun gptel-tools--editor-diagnostics (file-path)
  "Get editor diagnostics (errors, warnings, info) for workspaces.

FILE-PATH is the optional file path to get specific file diagnostics.
Leave empty for project-wide diagnostics.  Requires the file to be open
in the editor.

Returns a list of diagnostic objects in JSON format."
  (if (or (null file-path) (string-empty-p file-path))
      (mapcar #'gptel-tools--flymake-diag-to-json
	      (and (project-current) (flymake--project-diagnostics)))
    (if-let* ((buffer (get-file-buffer (expand-file-name file-path))))
	(with-current-buffer buffer
	  (mapcar #'gptel-tools--flymake-diag-to-json (flymake-diagnostics)))
      (error "File not opened in editor: %s" file-path))))


(defvar url-http-response-status)

(defvar gptel-tools-default-timeout 30)

(defun gptel-tools--url-retrieve (callback url &optional timeout)
  "Retrieve URL asynchronously and call CALLBACK with the result.

CALLBACK is a function that takes one argument, which is either:
- A list (:error MESSAGE [KEY VALUE]...) if an error occurred
- The buffer containing the retrieved content on success

URL is the URL to retrieve.

Optional TIMEOUT is the maximum time in seconds to wait for a response.
If the request takes longer than TIMEOUT seconds, it will be canceled
and CALLBACK will be called with a timeout error. If TIMEOUT is nil,
`gptel-tools-default-timeout' will be used.

Returns the process object for the URL retrieval."
  (let* ((timer nil)
         (process nil)
         (cb (lambda (result)
               (unwind-protect
                   (funcall callback result)
                 (when (timerp timer)
                   (cancel-timer timer)
                   (setq timer nil))
                 (when (processp process)
                   (delete-process process)
                   (setq process nil))
                 (setq callback nil)))))
    (when (null timeout) (setq timeout gptel-tools-default-timeout))
    (when (numberp timeout)
      (setq timer (run-at-time timeout nil
                               (lambda ()
                                 (when (processp process)
                                   (delete-process process))
                                 (funcall cb (list :error "Timed out"))))))
    (condition-case err
        (setq process (get-buffer-process (url-retrieve url cb)))
      (error (funcall cb (list :error "Failed to retrieve URL"
                               :internal-error (gptel--to-string err)))))))

(defun gptel-tools--read-url-async (callback url)
  "Call CALLBACK with parsed contents of URL, or (:error MESSAGE ...)."
  (gptel-tools--url-retrieve
   (lambda (status)
     (condition-case err
	 (if (eq (car-safe status) :error)
	     (funcall callback status)
	   (funcall callback
		    (progn
		      (goto-char (point-min))
		      (forward-paragraph)
		      (let ((dom (unwind-protect
				     (libxml-parse-html-region (point) (point-max))
				   (kill-buffer (current-buffer)))))
			(with-temp-buffer
			  (shr-insert-document dom)
			  (buffer-substring-no-properties (point-min) (point-max)))))))
       (error (funcall callback (list :error "Failed to read url"
				      :internal-error (gptel--to-string err))))))
   url))

(defun gptel-tools--insert-link-strip-href (dom)
  "Insert a link from DOM element.
If the link's href attribute is not from duckduckgo.com and is not
a relative or protocol-based URL, append the URL in parentheses."
  (with-no-warnings
    (shr-generic dom)
    (when-let* ((href (dom-attr dom 'href)))
      (or (string-match-p ".*duckduckgo\\.com.*" href)
	  (string-match-p "\\`\\(/\\|:\\)" href)
	  (shr-insert (format " (%s)" href))))))

(defvar shr-external-rendering-functions)
(defun gptel-tools--search-web-async (callback query)
  "Search DuckDuckGo for QUERY and call CALLBACK with formatted results."
  (gptel-tools--url-retrieve
   (lambda (status)
     (condition-case err
	 (cond
	  ((eq (car status) :error)
	   (funcall callback status))
	  ((>= url-http-response-status 400)
	   (error "HTTP error %s: %s"
		  url-http-response-status
		  (buffer-string)))
	  (t
	   (funcall callback
		    (progn
		      (goto-char (point-min))
		      (forward-paragraph)
		      (let
			  ((dom
			    (unwind-protect
				(libxml-parse-html-region
				 (point) (point-max))
			      (kill-buffer (current-buffer)))))
			(with-temp-buffer
			  (let
			      ((shr-external-rendering-functions
				'((a
				   . gptel-tools--insert-link-strip-href))))
			    (shr-insert-document
			     dom))
			  (buffer-substring-no-properties
			   (point-min) (point-max))))))))
       (error
	(funcall callback
		 (list :error
		       "Failed to fetch the URL"
		       :internal-error (gptel--to-string err))))))
   (format "https://html.duckduckgo.com/html/?q=%s" query)))


(defvar gptel-tools-jira-host nil)

(defun gptel-tools--get-jira-token ()
  (cl-assert gptel-tools-jira-host)
  (auth-info-password
   (car
    (or  (auth-source-search
	  :max 1
	  :host gptel-tools-jira-host)
	 (error "No authinfo for %s" gptel-tools-jira-host)))) )

(defun gptel-tools--search-jira-issues-async (callback jql max-results)
  (condition-case err
      (let* ((token (gptel-tools--get-jira-token))
	     (url (format "https://%s/rest/api/2/search?jql=%s&maxResults=%s"
			  gptel-tools-jira-host
			  (url-hexify-string jql)
			  max-results))
	     (url-request-extra-headers
	      `(("authorization" . ,(format "Bearer %s" token))
		("accept" . "application/json"))))
	(gptel-tools--url-retrieve
	 (lambda (status)
	   (if (eq (car-safe status) :error)
	       (funcall callback status)
	     (condition-case err1
		 (progn
		   (goto-char (point-min))
		   (forward-paragraph)
		   (let ((json-object-type 'plist))
		     (funcall callback (json-parse-buffer))))
	       (t (kill-buffer (current-buffer)))
	       (error (funcall callback (list :error "Failed to search issues"
					      :internal-error (gptel--to-string err1)))))))
	 url))
    (error (funcall callback (list :error "Failed to search jira"
				   :internal-error (gptel--to-string err))))))

(defun gptel-tools--get-jira-issue-async (callback issue-id)
  (condition-case err
      (let* ((token (gptel-tools--get-jira-token))
	     (url (format "https://%s/rest/api/2/issue/%s"
			  gptel-tools-jira-host
			  issue-id))
	     (url-request-extra-headers
	      `(("authorization" . ,(format "Bearer %s" token))
		("accept" . "application/json"))))
	(gptel-tools--url-retrieve
	 (lambda (status)
	   (if (eq (car-safe status) :error)
	       (funcall callback status)
	     (condition-case err1
		 (progn
		   (goto-char (point-min))
		   (forward-paragraph)
		   (let ((json-object-type 'plist))
		     (funcall callback (json-parse-buffer))))
	       (t (kill-buffer (current-buffer)))
	       (error (funcall callback (list :error "Failed to get issue"
					      :internal-error (gptel--to-string err1)))))))
	 url))
    (error (funcall callback (list :error "Failed to get issue"
				   :internal-error (gptel--to-string err))))))


(gptel-make-tool
 :name "read_file"
 :function #'gptel-tools--read-file
 :description "Read the contents of a file from the file system.
Use this tool when you need to examine the contents of a single file.
Use the `start` and `end` parameters to specify the region when you know the range.
You can also set `end` to -1 to read to the end of the file."
 :args (list '(:name "filepath"
		     :type string
		     :description "Path to the file to read. Supports relative paths and ~.")
	     '(:name "start"
		     :type number
		     :description "The first line to read (Start from 1)")
	     '(:name "end"
		     :type number
		     :description "The last line of the file to read.
This argument can also be -1, which means to read to the end of the file."))
 :confirm #'gptel-tools--read-file-confirm-p
 :category "coding-agent")

(gptel-make-tool
 :function #'gptel-tools--list-directory
 :name "list_directory"
 :description "List the contents of a given directory"
 :args (list '(:name "directory"
		     :type string
		     :description "The path to the directory to list"))
 :category "coding-agent")

(gptel-make-tool
 :name "create_file"
 :function #'gptel-tools--create-file
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
		     :type string
		     :description "The directory where to create the file")
	     '(:name "filename"
		     :type string
		     :description "The name of the file to create")
	     '(:name "content"
		     :type string
		     :description "The content to write to the file"))
 :category "coding-agent")

(gptel-make-tool
 :name "write_file"
 :function #'gptel-tools--write-file-async
 :async t
 :description "Write content to a file with user preview and confirmation.
This tool allows you to write new content to a file, overwriting any existing
content."
 :args (list '(:name "filepath"
		     :type string
		     :description "The path of the file to write")
	     '(:name "content"
		     :type string
		     :description "The content to write to the file. This will overwrite any existing content."))
 :category "coding-agent")

(gptel-make-tool
 :name "insert"
 :function #'gptel-tools--insert-async
 :async t
 :description "Insert text at a specific location in a file."
 :args (list (list :name "filepath"
		   :type 'string
		   :description "The path of the file to modify")
	     (list :name "insert_line"
		   :type 'number
		   :description "The line number AFTER which to insert the text (0 for beginning of file)")
	     (list :name "new_string"
		   :type 'string
		   :description "The text to insert"))
 :category "coding-agent")

(gptel-make-tool
 :name "str_replace"
 :function #'gptel-tools--string-replace-async
 :async t
 :description "Replace a specific string in a file with a new string.

You must use `read_file` to get the file's exact contents before editing.
The `old_string` must match the original content exactly, including all
indentation and newlines. The edit will fail if `old_string` is not unique.
Prefer small, targeted edits over large replacements.

If you are inserting text at a known position, use `insert` tool."
 :args (list '(:name "filepath"
		     :type string
		     :description "The path of the file to modify")
	     '(:name "old_string"
		     :type string
		     :description "The text to replace (MUST match exactly, including whitespaces and indentation)"
		     )
	     '(:name "new_string"
		     :type string
		     :description "The new text to insert in place of the old text"))
 :category "coding-agent")

(gptel-make-tool
 :name "grep"
 :function #'gptel-tools--grep-async
 :async t
 :description "Search for a pattern in the workspace.
Supports full regex syntax (eg. \"log.*Error\", \"function|var\\s+\\w+\", etc). "
 :args (list '(:name "regexp"
		     :type string
		     :description "The pattern to search.  It should be a regular expression.")
	     '(:name "working_dir"
		     :type string
		     :description "Optional: The directory in which to run the search. Defaults to the current directory if not specified."))
 :category "coding-agent")

(gptel-make-tool
 :name "shell_command"
 :function #'gptel-tools--shell-command-async
 :async t
 :description "Executes a shell command and returns the output. Use dedicated tools for file and search operations. Includes git commit process."
 :args (list
	'(:name "command"
		:type string
		:description "The complete shell command to execute.")
	'(:name "working_dir"
		:type string
		:description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
 :category "coding-agent"
 :confirm #'gptel-tools--shell-command-confirm-p)

(gptel-make-tool
 :name "editor_diagnostics"
 :description "Get editor diagnostics (errors, warnings, info) for workspaces.
Returns project-wide diagnostics when no file path is provided, or file-specific
diagnostics when a file path is given. Requires the file to be open in the editor."
 :function #'gptel-tools--editor-diagnostics
 :args (list '(:name "filepath"
		     :type string
		     :description "Optional file path to get specific file diagnostics. Leave empty for project-wide diagnostics."))
 :category "coding-agent")

(gptel-make-tool
 :name "read_url"
 :function #'gptel-tools--read-url-async
 :async t
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
		     :type string
		     :description "The URL to read"))
 :category "coding-agent")

(gptel-make-tool
 :name "search_web"
 :function #'gptel-tools--search-web-async
 :async t
 :description "Perform a web search using the DuckDuckGo search engine"
 :args (list '(:name "query"
		     :type string
		     :description "The search query string. "))
 :category "coding-agent")

(gptel-make-tool
 :name "get_date_time"
 :function (lambda () (shell-command-to-string "date"))
 :description "Get the current date and time."
 :category "coding-agent")


(gptel-make-tool
 :name "search_jira_issues"
 :function #'gptel-tools--search-jira-issues-async
 :async t
 :description "Search jira for issues"
 :args (list '(:name "jql"
		     :type string
		     :description "JQL query string to search issues")
	     '(:name "max_results"
		     :type integer
		     :description "Maximum number of results to return"))
 :category "jira")

(gptel-make-tool
 :name "get_jira_issue"
 :function #'gptel-tools--get-jira-issue-async
 :async t
 :description "Retrieve comprehensive information about a JIRA issue, \
including its summary, description, status, assignee and related details."
 :args (list '(:name "issue_id"
		     :type string
		     :description "The JIRA issue identifier (e.g., 'TPBUG-1007')"))
 :category "jira")

(provide 'gptel-tools)
;;; gptel-tools.el ends here
