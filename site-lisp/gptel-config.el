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
 :category "filesystem")

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
	    (when-let ((line-number (plist-get file-edit :line_number))
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

;;;

(setq gptel-backend (gptel-get-backend "DeepSeek"))
