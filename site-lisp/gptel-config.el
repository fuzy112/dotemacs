;; -*- lexical-binding: t; -*-

(require 'gptel)

;;; Models

(gptel-make-openai "DeepSeek"
  :host "api.deepseek.com"
  :endpoint "/chat/completions"
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models '(deepseek-chat deepseek-reasoner))

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

(defun my-gptel--edit_file (file-path file-edits)
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
		(ediff-buffers (find-file-noselect file-name) (current-buffer))
		(with-current-buffer (find-buffer-visiting file-name)
		  (save-buffer))
		(format "Successfully edited %s" file-name))
	    (format "Failed to edited %s" file-name))))
    (format "Failed to edited %s" file-path)))

(gptel-make-tool
 :name "edit_file"
 :function #'my-gptel--edit_file
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

(gptel-make-tool
 :name "run_command"
 :function (lambda (command &optional working_dir)
	     (with-temp-message (format "Executing command: `%s`" command)
	       (let ((default-directory (if (and working_dir (not (string= working_dir "")))
					    (expand-file-name working_dir)
					  default-directory)))
		 (shell-command-to-string command))))
 :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
 :args (list
	'(:name "command"
		:type string
		:description "The complete shell command to execute.")
	'(:name "working_dir"
		:type string
		:description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
 :category "command"
 :confirm t
 :include t)

(defun run_async_command (callback command)
  "Run COMMAND asynchronously and pass output to CALLBACK."
  (condition-case error
      (let ((buffer (generate-new-buffer " *async output*")))
	(with-temp-message (format "Running async command: %s" command)
	  (async-shell-command command buffer nil))
	(let ((proc (get-buffer-process buffer)))
	  (when proc
	    (set-process-sentinel
	     proc
	     (lambda (process _event)
	       (unless (process-live-p process)
		 (with-current-buffer (process-buffer process)
		   (let ((output (buffer-substring-no-properties (point-min) (point-max))))
		     (kill-buffer (current-buffer))
		     (funcall callback output)))))))))
    (t
     ;; Handle any kind of error
     (funcall callback (format "An error occurred: %s" error)))))

(gptel-make-tool
 :name "run_async_command"
 :function #'run_async_command
 :description "Run an async command."
 :args (list
        '(:name "command"
                :type "string"
		:description "Command to run."))
 :category "command"
 :async t
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

(defun gptel-read-documentation (symbol)
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
 :function #'gptel-read-documentation
 :description "Read the documentation for a given function or variable"
 :args (list '(:name "name"
	       :type string
	       :description "The name of the function or variable whose documentation is to be retrieved"))
 :category "emacs")

;; web tools

(gptel-make-tool
 :name "read_url"
 :function (lambda (url)
	     (with-current-buffer (url-retrieve-synchronously url)
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
    (with-current-buffer (url-retrieve-synchronously url)
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
		     :description "The search query string"))
 :category "web")

;; GitHub tools

(gptel-make-tool
 :name "get_pullreq"
 :function (lambda (pullreq)
	     (with-current-buffer
		 (forge-pullreq-setup-buffer
		  (forge-get-pullreq pullreq))
	       (buffer-substring-no-properties (point-min) (point-max))))
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
    (funcall +gptel-forge-post-callback "User canceled the post")
    (setq +gptel-forge-post-callback nil)))

(defun +gptel-forge-post-error ()
  (when +gptel-forge-post-callback
    (funcall +gptel-forge-post-callback "Error when submitting the post")
    (setq +gptel-forge-post-callback nil)))

(defun +gptel-forge-post-submitted ()
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
 :description "Approve the pull request"
 :args (list '(:name "pullreq_id"
		     :type integer
		     :description "The id of the pull request")
	     '(:name "comments"
		     :type string
		     :description "Comments to post to the pull request.
Note that the user will get a chance to edit the comments."))
 :category "github")

(defun +gptel-request-changes (callback pull-number comments)
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
      (forge-request-changes))))


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
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (forward-paragraph)
      (let ((json-object-type 'hash-table))
	(json-parse-buffer)))))

(gptel-make-tool
 :name "get_jira_issue"
 :function #'+gptel-get-jira-issue
 :description "Read the details of a JIRA issue"
 :args '(( :name "issue_id"
	   :type string
	   :description "The ID or key of the issue to be read"))
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
    (with-current-buffer (url-retrieve-synchronously url)
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
    (with-current-buffer (url-retrieve-synchronously url)
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
    (with-current-buffer (url-retrieve-synchronously url)
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
    (with-current-buffer (url-retrieve-synchronously url)
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

;;;

(defun +gptel-auto-scroll-safe ()
  (ignore-errors
    (gptel-auto-scroll)))

(add-hook 'gptel-post-stream-hook #'+gptel-auto-scroll-safe)
