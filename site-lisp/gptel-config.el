;; -*- lexical-binding: t; -*-

(require 'gptel)
(require 'url-http)

;;; Models

(gptel-make-openai "Moonshot"
  :host "api.moonshot.cn"
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models '((kimi-latest
	     :description "The latest model used by Kimi Assistant"
	     :capabilities (media tool-use json)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 128
	     :input-cost 1.50
	     :output-cost 4.50)
	    (kimi-thinking-preview
	     :description "The Kimi reasoning model"
	     :capabilities (reasoning media)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 128
	     :input-cost 30
	     :output-cost 30)
	    (kimi-k2-0711-preview
	     :description "A model suitable for coding"
	     :capabilities (media tool-use json)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 128
	     :input-cost 0.15
	     :output-cost 0.60)
	    (moonshot-v1-auto
	     :description "The standard Moonshot V1 model"
	     :capabilities (tool-use json)
	     :context-window 128
	     :input-cost 1.50
	     :output-cost 4.50))
  :request-params '(:tools [(:type "builtin_function" :function (:name "$web_search"))]))

(gptel-make-deepseek "DeepSeek"
  :stream t
  :key #'gptel-api-key-from-auth-source)

(gptel-make-kagi "Kagi"
  :key #'gptel-api-key-from-auth-source)

(gptel-make-gh-copilot "Copilot")

;;; Tools

;; filesystem tools

(gptel-make-tool
 :name "read_file"
 :function (lambda (filepath)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name filepath))
	       (buffer-string)))
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath"
		     :type string
		     :description "Path to the file to read. Supports relative paths and ~."))
 :category "filesystem"
 :confirm t)

(gptel-make-tool
 :function (lambda (directory)
	     (mapconcat #'identity
			(directory-files directory)
			"\n"))
 :name "list_directory"
 :description "List the contents of a given directory"
 :args (list '(:name "directory"
	       :type string
	       :description "The path to the directory to list"))
 :category "filesystem")


(gptel-make-tool
 :name "create_file"
 :function (lambda (path filename content)
	     (let ((full-path (expand-file-name filename path)))
	       (with-temp-buffer
		 (insert content)
		 (write-file full-path))
	       (format "Created file %s in %s" filename path)))
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
 :category "filesystem")

(defun +gptel-edit-file (file-path file-edits)
  "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
  (if (and file-path (not (string= file-path "")) file-edits)
      (with-current-buffer (get-buffer-create "*edit-file*")
	(erase-buffer)
	(insert-file-contents (expand-file-name file-path))
	(let ((inhibit-read-only t)
	      (case-fold-search nil)
	      (file-name (expand-file-name file-path))
	      (edit-success nil))
	  ;; apply changes
	  (dolist (file-edit (seq-into file-edits 'list))
	    (when-let* ((line-number (plist-get file-edit :line_number))
			(old-string (plist-get file-edit :old_string))
			(new-string (plist-get file-edit :new_string))
			(is-valid-old-string (not (string= old-string ""))))
	      (goto-char (point-min))
	      (forward-line (1- line-number))
	      (when (search-forward old-string nil t)
		(replace-match new-string t t)
		(setq edit-success t))))
	  ;; return result to gptel
	  (if edit-success
	      (progn
		;; show diffs
		(save-window-excursion
		  (ediff-buffers (find-file-noselect file-name) (current-buffer))
		  (recursive-edit))
		(with-current-buffer (find-buffer-visiting file-name)
		  (save-buffer))
		(format "Successfully edited %s" file-name))
	    (format "Failed to edited %s" file-name))))
    (format "Failed to edited %s" file-path)))

(gptel-make-tool
 :name "edit_file"
 :function #'+gptel-edit-file
 :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
 :args (list '(:name "file-path"
		     :type string
		     :description "The full path of the file to edit")
	     '(:name "file-edits"
		     :type array
		     :items (:type object
				   :properties
				   (:line_number
				    (:type integer :description "The line number of the file where edit starts.")
				    :old_string
				    (:type string :description "The old-string to be replaced.")
				    :new_string
				    (:type string :description "The new-string to replace old-string.")))
		     :description "The list of edits to apply on the file"))
 :category "filesystem")

;; command tools

(defun +gptel-run-command-sandboxed (command &optional working_dir)
  (with-temp-message (format "Executable command: `%s'" command)
    (let ((dir (expand-file-name default-directory))
	  (default-directory (if (and working_dir (not (string= working_dir "")))
				 (expand-file-name working_dir)
			       default-directory)))
      (with-temp-buffer
	(process-file "/usr/bin/bwrap" nil t nil
		      "--ro-bind" "/lib" "/lib"
		      "--ro-bind" "/lib64" "/lib64"
		      "--ro-bind" "/bin" "/bin"
		      "--ro-bind" "/usr" "/usr"
		      "--dev" "/dev"
		      "--bind" dir dir
		      "--new-session"
		      "--unshare-all"
		      "--share-net"
		      "--chdir" default-directory
		      "bash" "-c" command)
	(buffer-string)))))

(gptel-make-tool
 :name "run_command"
 :function #'+gptel-run-command-sandboxed
 :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run.
This tool is not meant to be used to modify files: use `edit_file` to do that."
 :args (list
	'(:name "command"
		:type string
		:description "The complete shell command to execute.")
	'(:name "working_dir"
		:type string
		:description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
 :category "command"
 ;; :confirm t
 :include t)

;; Emacs tools

(gptel-make-tool
 :name "echo_message"
 :function (lambda (text)
	     (message "%s" text)
	     (format "Message sent: %s" text))
 :description "Send a message to the *Messages* buffer"
 :args (list '(:name "text"
	       :type string
	       :description "The text to send to the messages buffer"))
 :category "emacs")

(defun +gptel-read-documentation (symbol)
  "Read the documentation for SYMBOL, which can be a function or variable."
  (let ((sym (intern symbol)))
    (cond
     ((fboundp sym)
      (documentation sym))
     ((boundp sym)
      (documentation-property sym 'variable-documentation))
     (t
      (format "No documentation found for %s" symbol)))))

(gptel-make-tool
 :name "read_documentation"
 :function #'+gptel-read-documentation
 :description "Read the documentation for a given function or variable"
 :args (list '(:name "name"
	       :type string
	       :description "The name of the function or variable whose documentation is to be retrieved"))
 :category "emacs")

(defun +gptel-search-emacs-lists  (query)
  (with-current-buffer
      (+gptel-url-retrieve
       (format "https://yhetil.org/emacs/?q=%s" query))
    (buffer-substring-no-properties (point-min) (point-max))))

(gptel-make-tool
 :name "search_emacs_mailing_list"
 :function #'+gptel-search-emacs-lists
 :description "Search an online archive of Emacs-related mailing lists."
 :args (list '(:name "query"
		     :type string
		     :description "The search query string"))
 :category "emacs")


(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer-name)
	     (if (string-match-p "\\`\\(?: \\|\\*\\|\\.\\)" buffer-name)
		 (error "Buffer unreadable: %s" buffer-name))
	     (with-current-buffer buffer-name
	       (message "Reading buffer %s..." buffer-name)
	       (buffer-substring-no-properties (point-min) (point-max))))
 :description "Read the content of an Emacs buffer."
 :args (list '(:name "buffer_name"
		     :type string
		     :description "The name of the buffer to read"))
 :category "emacs")


(defun +gptel-edit-buffer (buffer-name buffer-edits)
  "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
  (if (and buffer-name (not (string= buffer-name "")) buffer-edits)
      (with-current-buffer (get-buffer-create "*edit-file*")
	(erase-buffer)
	(insert-buffer-substring buffer-name)
	(let ((inhibit-read-only t)
	      (case-fold-search nil)
	      ;; (file-name (expand-file-name buffer-name))
	      (edit-success nil))
	  ;; apply changes
	  (dolist (buffer-edit (seq-into buffer-edits 'list))
	    (when-let* ((line-number (plist-get buffer-edit :line_number))
			(old-string (plist-get buffer-edit :old_string))
			(new-string (plist-get buffer-edit :new_string))
			(is-valid-old-string (not (string= old-string ""))))
	      (goto-char (point-min))
	      (forward-line (1- line-number))
	      (when (search-forward old-string nil t)
		(replace-match new-string t t)
		(setq edit-success t))))
	  ;; return result to gptel
	  (if edit-success
	      (progn
		;; show diffs
		(ediff-buffers buffer-name (current-buffer))
		(format "Successfully edited %s" buffer-name))
	    (format "Failed to edited %s" buffer-name))))
    (format "Failed to edited %s" buffer-name)))


(gptel-make-tool
 :name "edit_buffer"
 :function #'+gptel-edit-buffer
 :description "Edit buffer with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
 :args (list '(:name "buffer_name"
		     :type string
		     :description "The full path of the file to edit")
	     '(:name "buffer_edits"
		     :type array
		     :items (:type object
				   :properties
				   (:line_number
				    (:type integer :description "The line number of the file where edit starts.")
				    :old_string
				    (:type string :description "The old-string to be replaced.")
				    :new_string
				    (:type string :description "The new-string to replace old-string.")))
		     :description "The list of edits to apply on the buffer"))
 :category "emacs")


;; web tools

(defvar url-http-response-status)

(defun +gptel-url-retrieve (url)
  (message "Retrieving %s..." url)
  (let ((buffer (url-retrieve-synchronously url t t 20)))
    (unless buffer
      "Retrieving %s...failed" url)
    (with-current-buffer buffer
      (message "Retrieving %s...%s" url url-http-response-status)
      (when (>= url-http-response-status 400)
	(error "HTTP Error %s: %s" url-http-response-status
	       (with-current-buffer buffer
		 (buffer-string)))))
    buffer))

(gptel-make-tool
 :name "read_url"
 :function (lambda (url)
	     (with-current-buffer (+gptel-url-retrieve url)
	       (goto-char (point-min))
	       (forward-paragraph)
	       (let ((dom (libxml-parse-html-region (point) (point-max))))
		 (run-at-time 0 nil #'kill-buffer (current-buffer))
		 (with-temp-buffer
		   (shr-insert-document dom)
		   (buffer-substring-no-properties (point-min) (point-max))))))
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
		     :type string
		     :description "The URL to read"))
 :category "web")

(defun +gptel-insert-link (dom)
  (shr-generic dom)
  (when-let* ((href (dom-attr dom 'href)))
    (or (string-match-p ".*duckduckgo\\.com.*" href)
	(string-match-p "\\`\\(/\\|:\\)" href)
	(shr-insert (format " (%s)" href)))))

(defvar shr-external-rendering-functions)
(defun +gptel-search-ddg (query)
  (let ((url (format "https://html.duckduckgo.com/html/?q=%s" query)))
    (with-current-buffer (+gptel-url-retrieve url)
      (goto-char (point-min))
      (forward-paragraph)
      (let ((dom (libxml-parse-html-region (point) (point-max))))
	(run-at-time 0 nil #'kill-buffer (current-buffer))
	(with-temp-buffer
	  (let ((shr-external-rendering-functions '((a . +gptel-insert-link))))
	    (shr-insert-document dom))
	  (buffer-substring-no-properties (point-min) (point-max)))))))

(gptel-make-tool
 :name "search_web"
 :function #'+gptel-search-ddg
 :description "Perform a web search using the DuckDuckGo search engine"
 :args (list '(:name "query"
		     :type string
		     :description "The search query string.  When searching the web, one should always use English rather than their native language."))
 :category "web")

(gptel-make-tool
 :name "$web_search"
 :function (lambda (&optional search_result)
	     (json-serialize
	      `(:search_result ,search_result)))
 :description "Moonshot builtin web search.  Only usable by moonshot model (kimi), ignore this if you are not."
 :args '((:name "search_result" :type object :optional t))
 :category "web")

;; GitHub tools

(gptel-make-tool
 :name "get_pullreq"
 :function (lambda (pullreq)
	     (save-excursion
	       (with-current-buffer
		   (forge-pullreq-setup-buffer
		    (forge-get-pullreq pullreq))
		 (buffer-substring-no-properties (point-min) (point-max)))))
 :description "Get the details of a pull request"
 :args (list '(:name "pullreq_id"
		     :type integer
		     :description "The id of the pull request"))
 :category "github")

(defvar-local +gptel-forge-post-callback nil)
(defvar +gptel-forge-post-cancel-hook nil)
(defvar +gptel-forge-post-submit-error-hook nil)

(define-advice forge-post-cancel (:before () +gptel-hook)
  (run-hooks '+gptel-forge-post-cancel-hook))

(define-advice forge--post-submit-errorback (:before (&rest args) +gptel-hook)
  (run-hooks '+gptel-forge-post-submit-error-hook))

(defun +gptel-forge-post-cancel ()
  (when +gptel-forge-post-callback
    (funcall +gptel-forge-post-callback "User manually rejected the pull request.  Do not continue.")
    (setq +gptel-forge-post-callback nil)))

(defun +gptel-forge-post-error ()
  (when +gptel-forge-post-callback
    (funcall +gptel-forge-post-callback "Error when submitting the post")
    (setq +gptel-forge-post-callback nil)))

(defun +gptel-forge-post-submitted (&rest _)
  (when +gptel-forge-post-callback
    (funcall +gptel-forge-post-callback "Successfully submitted the post")
    (setq +gptel-forge-post-callback nil)))

(defun +gptel-approve-pullreq (callback pull-number comments)
  (with-current-buffer
      (forge-pullreq-setup-buffer (forge-get-pullreq pull-number))
    (let ((forge-edit-post-hook forge-edit-post-hook))
      (push (lambda ()
	      (insert "### The following message is generated by GitHub Copilot\n\n")
	      (insert comments)
	      (insert "\n\n### \n\n")
	      (setq +gptel-forge-post-callback callback)
	      (add-hook '+gptel-forge-post-cancel-hook
			#'+gptel-forge-post-cancel
			nil t)
	      (add-hook '+gptel-forge-post-submit-error-hook
			#'+gptel-forge-post-error
			nil t)
	      (add-hook 'forge-post-submit-callback-hook
			#'+gptel-forge-post-submitted
			nil t))
	    forge-edit-post-hook)
      (forge-approve-pullreq))))

(gptel-make-tool
 :name "approve_pullreq"
 :function #'+gptel-approve-pullreq
 :async t
 :description "Approve a GitHub pull request and add an optional review comment. This action marks the pull request as approved by the authenticated user and posts the provided comments as part of the review."
 :args (list '(:name "pullreq_id"
		     :type integer
		     :description "The numeric ID of the GitHub pull request to approve")
	     '(:name "comments"
		     :type string
		     :description "Optional review comments to post alongside the approval.
The text will be presented in an editor buffer for review and modification before submission."))
 :category "github")

(defun +gptel-request-changes (callback pull-number comments)
  (condition-case error
      (with-current-buffer
	  (forge-pullreq-setup-buffer (forge-get-pullreq pull-number))
	(let ((forge-edit-post-hook forge-edit-post-hook))
	  (push (lambda ()
		  (condition-case error
		      (progn
			(insert "### The following message is generated by GitHub Copilot\n\n")
			(insert comments)
			(insert "\n\n### \n\n")
			(setq +gptel-forge-post-callback callback)
			(add-hook '+gptel-forge-post-cancel-hook
				  #'+gptel-forge-post-cancel
				  nil t)
			(add-hook '+gptel-forge-post-submit-error-hook
				  #'+gptel-forge-post-error
				  nil t)
			(add-hook 'forge-post-submit-callback-hook
				  #'+gptel-forge-post-submitted
				  nil t))
		    (t
		     (funcall callback (format "An error occurred: %s" error))
		     (setq callback nil))))
		forge-edit-post-hook)
	  (forge-request-changes)))
    (t
     (funcall callback (format "An error occurred: %s" error)))))

(gptel-make-tool
 :name "request_changes"
 :function #'+gptel-request-changes
 :async t
 :description "Request changes on the pull request"
 :args (list '(:name "pullreq_id"
		     :type integer
		     :description "The id of the pull request")
	     '(:name "comments"
		     :type string
		     :description "Comments to post to the pull request.
Note that the user will get a chance to edit the comments."))
 :category "github")


;; JIRA tools

(defvar +gptel-jira-host nil)

(defun +auth-source-get-jira-token ()
  (auth-info-password
   (car
    (or  (auth-source-search
	  :max 1
	  :host +gptel-jira-host)
	 (error "No authinfo for %s" +gptel-jira-host)))))

(defun +gptel-get-jira-issue  (issue-id)
  (let* ((token (+auth-source-get-jira-token))
	 (url (format "https://%s/rest/api/2/issue/%s"
		      +gptel-jira-host issue-id))
	 (url-request-extra-headers
	  `(("authorization" . ,(format "Bearer %s" token))
	    ("accept" . "application/json"))))
    (with-current-buffer (+gptel-url-retrieve url)
      (run-at-time 0 nil #'kill-buffer (current-buffer))
      (goto-char (point-min))
      (forward-paragraph)
      (let ((json-object-type 'hash-table))
	(json-parse-buffer)))))

(gptel-make-tool
 :name "get_jira_issue"
 :function #'+gptel-get-jira-issue
 :description "Retrieve comprehensive information about a JIRA issue, including its summary, description, status, assignee, and related details from the JIRA API"
 :args '((:name "issue_id"
	  :type string
	  :description "The JIRA issue identifier (e.g., 'PROJ-123')"))
 :category "jira")

(defun +gptel-create-jira-issue (project-key summary description issue-type)
  (let* ((token (+auth-source-get-jira-token))
	 (url (format "https://%s/rest/api/2/issue" +gptel-jira-host))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  `(("authorization" . ,(format "Bearer %s" token))
	    ("content-type" . "application/json")
	    ("accept" . "application/json")))
	 (url-request-data
	  (json-encode
	   `(("fields" .
	      (("project" . (("key" . ,project-key)))
	       ("summary" . ,summary)
	       ("description" . ,description)
	       ("issuetype" . (("name" . ,issue-type)))))))))
    (with-current-buffer (+gptel-url-retrieve url)
      (run-at-time 0 nil #'kill-buffer (current-buffer))
      (goto-char (point-min))
      (forward-paragraph)
      (let ((json-object-type 'hash-table))
	(json-parse-buffer)))))

(gptel-make-tool
 :name "create_jira_issue"
 :function #'+gptel-create-jira-issue
 :description "Create a new JIRA issue"
 :args (list '(:name "project_key"
		     :type string
		     :description "The project key where issue will be created")
	     '(:name "summary"
		     :type string
		     :description "Summary/title of the issue")
	     '(:name "description"
		     :type string
		     :description "Description of the issue")
	     '(:name "issue_type"
		     :type string
		     :description "Type of issue (Bug, Task, etc.)"))
 :category "jira")

(defun +gptel-comment-jira-issue (issue-id comment)
  (let* ((token (+auth-source-get-jira-token))
	 (url (format "https://%s/rest/api/2/issue/%s/comment"
		      +gptel-jira-host issue-id))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  `(("authorization" . ,(format "Bearer %s" token))
	    ("content-type" . "application/json")
	    ("accept" . "application/json")))
	 (url-request-data
	  (json-encode `(("body" . ,comment)))))
    (with-current-buffer (+gptel-url-retrieve url)
      (run-at-time 0 nil #'kill-buffer (current-buffer))
      (goto-char (point-min))
      (forward-paragraph)
      (let ((json-object-type 'hash-table))
	(json-parse-buffer)))))

(gptel-make-tool
 :name "comment_jira_issue"
 :function #'+gptel-comment-jira-issue
 :description "Add a comment to a JIRA issue"
 :args (list '(:name "issue_id"
		     :type string
		     :description "The ID or key of the issue")
	     '(:name "comment"
		     :type string
		     :description "Comment text to add to the issue"))
 :category "jira"
 :confirm t)

(defun +gptel-transition-jira-issue (issue-id transition-id)
  (let* ((token (+auth-source-get-jira-token))
	 (url (format "https://%s/rest/api/2/issue/%s/transitions"
		      +gptel-jira-host issue-id))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  `(("authorization" . ,(format "Bearer %s" token))
	    ("content-type" . "application/json")
	    ("accept" . "application/json")))
	 (url-request-data
	  (json-encode `(("transition" . (("id" . ,transition-id)))))))
    (with-current-buffer (+gptel-url-retrieve url)
      (run-at-time 0 nil #'kill-buffer (current-buffer))
      (if (eq url-http-response-status 204)
	  (format "Successfully transitioned issue %s" issue-id)
	(buffer-string)))))

(gptel-make-tool
 :name "transition_jira_issue"
 :function #'+gptel-transition-jira-issue
 :description "Transition a JIRA issue to a new status"
 :args (list '(:name "issue_id"
		     :type string
		     :description "The ID or key of the issue")
	     '(:name "transition_id"
		     :type string
		     :description "The ID of the transition to perform"))
 :category "jira"
 :confirm t)

(defun +gptel-search-jira-issues (jql max-results)
  (let* ((token (+auth-source-get-jira-token))
	 (url (format "https://%s/rest/api/2/search?jql=%s&maxResults=%s"
		      +gptel-jira-host
		      (url-hexify-string jql)
		      max-results))
	 (url-request-extra-headers
	  `(("authorization" . ,(format "Bearer %s" token))
	    ("accept" . "application/json"))))
    (with-current-buffer (+gptel-url-retrieve url)
      (run-at-time 0 nil #'kill-buffer (current-buffer))
      (goto-char (point-min))
      (forward-paragraph)
      (let ((json-object-type 'hash-table))
	(json-parse-buffer)))))

(gptel-make-tool
 :name "search_jira_issues"
 :function #'+gptel-search-jira-issues
 :description "Search JIRA issues with JQL"
 :args (list '(:name "jql"
		     :type string
		     :description "JQL query string to search issues")
	     '(:name "max_results"
		     :type integer
		     :description "Maximum number of results to return"))
 :category "jira")

;;; Tweaks

(defun +gptel-auto-scroll-safe ()
  (ignore-errors
    (setq-local scroll-preserve-screen-position t)
    (gptel-auto-scroll)))

(add-hook 'gptel-post-stream-hook #'+gptel-auto-scroll-safe)

(defun +gptel-remove-markdown-code-fences (beg end)
  "Remove markdown code fences from the given region.
Specifically, this function will remove the '```' markers at the
beginning and end of the region, if they exist.

BEG and END define the region to process."
  (let ((beg (copy-marker beg))
	(end (copy-marker end)))
    (goto-char beg)
    (when (looking-at (rx bol "```" (zero-or-more not-newline) "\n"))
      (replace-match ""))
    (goto-char end)
    (when (looking-back (rx bol "```" eol) (line-beginning-position))
      (replace-match ""))))

(add-hook 'gptel-post-rewrite-functions #'+gptel-remove-markdown-code-fences)

(keymap-set gptel-mode-map "C-c k" #'gptel-abort)

;;; Presets

(gptel-make-preset 'deepseek-translator
  :description "High-precision English ↔ Chinese translator"
  :system "You are a professional English-Chinese translator.
- Translate accurately, preserving tone, nuance, and context.
- Return only the translation, no additional commentary.
- Use concise, idiomatic language.
- Retain proper nouns, code, and formatting exactly as given."
  :backend "DeepSeek"
  :model 'deepseek-chat
  :use-tools nil
  :temperature 0.1
  :stream t)

(gptel-make-preset 'kimi-assistant
  :description "Kimi with web search and URL reading"
  :backend "Moonshot"
  :model 'kimi-latest
  :stream t
  :temperature 0.6
  :use-tools 'force
  :tools '("$web_search" "read_url" "read_documentation" "search_emacs_mailing_list")
  :system "You are Kimi, an Emacs-embedded LLM assistant.
Be concise, accurate, and helpful.
You may search the web or read URLs when needed.
Whenever you cite external information, always include the full source URL.")

(gptel-make-preset 'kimi-coder
  :description "Fast, deterministic coding assistant using Moonshot’s Kimi-k2-0711-preview"
  :backend "Moonshot"
  :model 'kimi-k2-0711-preview
  :stream t
  :temperature 0.1
  :max-tokens 4096
  :use-tools nil
  :system-message "You are an expert Emacs-Lisp and general-purpose programmer. Provide
concise, correct, and idiomatic code. Prefer built-ins and avoid
external dependencies unless necessary. Always return complete, runnable
snippets.")

(gptel-make-preset 'kimi-agent
  :description "Elite coding agent powered by Moonshot Kimi-k2-0711-preview"
  :backend "Moonshot"
  :model 'kimi-k2-0711-preview
  :stream t
  :temperature 0.1
  :max-tokens 8192
  :use-tools t
  :tools '("read_file" "run_command" "list_directory" "shellcheck" "edit_file")
  :system-message "You are an expert Emacs-Lisp and polyglot programmer. Respond with
minimal, idiomatic, and fully-functional code. Favor built-ins and avoid
external dependencies. Always return complete, runnable snippets. When
editing, output only the changed portions with clear context.")

(gptel-make-preset 'deepseek-reasoner
  :description "DeepSeek Reasoner – step-by-step reasoning assistant"
  :backend "DeepSeek"
  :model 'deepseek-reasoner
  :stream t
  :temperature 0.3
  :max-tokens 8192
  :use-tools nil
  :system-message "You are DeepSeek Reasoner. Think step-by-step, expose your
chain-of-thought, and verify every conclusion before presenting the
final answer.")

(gptel-make-preset 'kagi-search
  :description "Kagi search assistant"
  :backend "Kagi"
  :stream t
  :temperature 0.5
  :use-tools nil
  :system-message "You are a search assistant powered by Kagi. Provide accurate, concise
answers based on search results. Always cite sources when possible.")

;;; Commands

(defun +gptel-review-pullreq (pullreq)
  "Initiate a review session for a pull request using GPEL.

PULLREQ can be a forge pull request object or nil, in which case the
function will prompt the user to select a pull request to review.

The review session will open in a dedicated buffer where the user can
compose their review comments. The session is configured with a specific
backend, model, and tools to assist with the review process. The system
message sets the context for the review, emphasizing thoroughness and high
standards.

The function inserts a prompt for the user to review the pull request,
guiding them to consider code quality, commit quality, and JIRA compliance.
It then sends the prompt to the GPT backend for processing."
  (interactive (list (or (forge-current-pullreq)
			 (forge-get-pullreq (forge-read-pullreq "Pull-request: ")))))
  (let* ((session-buffer (gptel (format "*Review on PR #%s*" (oref pullreq number)))))
    (pop-to-buffer session-buffer)
    (with-current-buffer session-buffer
      (setq-local gptel-backend (gptel-get-backend "Copilot")
		  gptel-model 'claude-3.7-sonnet
		  gptel-tools (append (gptel-get-tool "filesystem")
				      (gptel-get-tool "github")
				      (gptel-get-tool "jira")
				      (gptel-get-tool "command"))
		  gptel--system-message "You are an experienced senior developer and a strict code reviewer with high standards.
Your role is to thoroughly review pull requests focusing on:
1. Code Quality:
   - Clean code principles
   - Performance implications
   - Security considerations
   - Design patterns and architecture
   - Test coverage
2. Commit Quality:
   - Clear and descriptive commit messages
   - Logical commit history
   - Appropriate commit size
3. JIRA Compliance:
   - Ticket description matches implementation
   - All acceptance criteria met
   - Proper ticket status and linking

Provide specific, actionable feedback and don't hesitate to request changes if standards aren't met.
Remember to be thorough, constructive, and maintain high quality standards!")
      (insert (format "Please review pull request #%s thoroughly.\n\n" (oref pullreq number)))
      (insert "Consider:

1. Is the code implementation clean, efficient, and secure?
2. Are commit messages clear and history well-structured?
3. Does the implementation fully satisfy the JIRA ticket requirements?

Provide your detailed review with specific recommendations for improvement if needed.")
      (gptel-send))))

(gptel-make-tool
  :name "shellcheck"
  :function (lambda (filename)
	      (with-temp-buffer
		(process-file "shellcheck" nil t nil
			      (expand-file-name filename) "--exclude=SC1091,SC2034")
		(buffer-string)))
  :description "Run shellcheck to analyze shell scripts for errors, bugs, and potential pitfalls. Excludes specific checks for source files (SC1091) and unused variables (SC2034)."
  :args (list '(:name "filename"
		      :type string
		      :description "Path to the shell script file to be analyzed."))
  :category "command"
  :include t)

(defun +gptel-shellcheck-fix (file)
  (interactive
   (list (or (and current-prefix-arg (read-file-name "File: "))
	     (buffer-file-name))))
  (with-current-buffer (find-buffer-visiting file)
    (set (make-local-variable 'flymake-show-diagnostics-at-end-of-line) t)
    (flymake-mode 1))
  (let* ((short-name (file-name-nondirectory file))
	 (gpt-buf (gptel (format "*ShellCheck/%s*" short-name))))
    (pop-to-buffer gpt-buf)
    (with-current-buffer gpt-buf
      (setq-local gptel-backend (gptel-get-backend "Copilot"))
      (setq-local gptel-model 'claude-3.7-sonnet)
      (setq-local gptel-tools
		  (append (gptel-get-tool "filesystem")
			  (gptel-get-tool "web")
			  (list (gptel-get-tool "shellcheck"))))
      (setq-local gptel-confirm-tool-calls nil)
      (setq-local gptel--system-message
		  "You are an expert shell script developer focusing on robustness and security.
Your role is to analyze and improve shell scripts by:

1. Interpreting shellcheck output with expert judgment
2. Fixing identified issues while preserving script functionality
3. Using directive comments (#shellcheck disable=RULES) judiciously when:
   - The rule produces false positives
   - The fix would significantly impair readability or maintainability
   - The current implementation is intentional and secure

For directive comments:
- Place them at file start (after headers/modelines but before commands)
  OR immediately before affected commands/blocks
- Always include clear explanations for why rules are disabled
- Consider long-term maintenance implications

Provide your analysis with security-focused improvements while maintaining
script readability and reliability.")
      (insert (format "Please analyze and improve the shell script '%s':

1. Run shellcheck and examine its output
2. Provide fixes for legitimate issues
3. Where shellcheck rules need to be disabled, explain the rationale
4. Ensure all changes preserve the script's original functionality
5. Use the `edit_file' tool to edit the script
"
		     file))
      (gptel-send))))

(provide 'gptel-config)
;;; gptel-config.el ends here
