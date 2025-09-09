;;; gptel-config.el --- Configuration for gptel AI assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zhengyi Fu <i@fuzy.me>

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

;; This file provides comprehensive configuration for gptel, an AI assistant
;; integration for Emacs.  It defines multiple AI backends (Moonshot, DeepSeek,
;; Kagi, GitHub Copilot), various tools for file operations, web search, GitHub
;; integration, JIRA operations, and todo list management.  The configuration
;; includes specialized presets for different use cases like translation,
;; coding, mathematics, and general assistance.

;; The configuration emphasizes:
;; - Multiple AI backend support with proper API key management
;; - Extensive tool integration for filesystem, web, GitHub, and JIRA operations
;; - Project-specific todo list management
;; - Specialized presets for different AI assistant roles
;; - Proper error handling and user confirmation for sensitive operations

;;; Code:

(require 'gptel)
(require 'url-http)
(require 'with-editor)
(require 'flymake)
(require 'dom)

;;; Reset backends, tools, and presets

(setq gptel--known-backends nil
      gptel--known-tools nil
      gptel--known-presets nil)

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
	     :output-cost 0.58)
	    (kimi-k2-turbo-preview
	     :description "A model suitable for coding"
	     :capabilities (media tool-use json)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 128
	     :input-cost 0.15
	     :output-cost 0.58)
	    (moonshot-v1-auto
	     :description "The standard Moonshot V1 model"
	     :capabilities (tool-use json)
	     :context-window 128
	     :input-cost 1.50
	     :output-cost 4.50))
  ;; :request-params '(:tools [(:type "builtin_function" :function (:name "$web_search"))])
  )

(gptel-make-deepseek "DeepSeek"
  :stream t
  :key #'gptel-api-key-from-auth-source)

(gptel-make-kagi "Kagi"
  :key #'gptel-api-key-from-auth-source)

(gptel-make-gh-copilot "Copilot")

(setq-default gptel-backend (gptel-get-backend "DeepSeek"))
(setq-default gptel-model 'deepseek-chat)

;;; Tool

;; filesystem tools

(gptel-make-tool
 :name "read_file"
 :function (lambda (filepath start end)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name filepath))
	       (goto-char (point-min))
	       (forward-line (1- start))
	       (let ((start-pos (point-marker)))
		 (if (zerop end)
		     (buffer-substring-no-properties
		      start-pos (point-max))
		   (goto-char (point-min))
		   (forward-line (1- end))
		   (buffer-substring-no-properties
		    start-pos (line-end-position))))))
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
	       (if (file-exists-p full-path)
		   (error "File %s already exists" filename)
		 (with-temp-buffer
		   (insert content)
		   (write-file full-path))
		 (format "Created file %s in %s" filename path))))
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

(defun +gptel-write-file-async (callback file-path content)
  "Asynchronously write CONTENT to FILE-PATH with user preview.

CALLBACK is a function that will be called with the result message upon
completion.  The callback receives a string describing the outcome.
FILE-PATH is the path to the file to be written.
CONTENT is the new content to write to the file.

The function will:
1. Create a preview buffer showing the new content
2. Allow the user to review and optionally modify the content
3. Provide options to accept, reject, or request changes
4. Write the content to the file upon user acceptance

Uses `gptel-abort' for proper error handling within gptel sessions.

This function is designed for use with gptel-mode and provides an
interactive workflow for file writing operations.

(fn CALLBACK FILE-PATH CONTENT)"
  (cl-assert gptel-mode)
  (condition-case err
      (let* ((preview-buffer (generate-new-buffer "*write-file-preview*")))
	(with-current-buffer preview-buffer
	  (insert content))
	(+gptel-ediff-file-with-buffer file-path preview-buffer callback))
    (error (funcall callback (format "An error occurred: %S" err))
	   (signal (car err) (cdr err)))))

(gptel-make-tool
 :name "write_file"
 :function #'+gptel-write-file-async
 :async t
 :description "Write content to a file with user preview and confirmation.
This tool allows you to write new content to a file, overwriting any existing
content."
 :args (list '(:name "file_path"
		     :type string
		     :description "The full path of the file to write")
	     '(:name "content"
		     :type string
		     :description "The content to write to the file. This will overwrite any existing content."))
 :category "filesystem")

(defvar ediff-window-setup-function)

(declare-function ediff-setup-windows-plain "ediff-wind.el" (arg1 arg2 arg3 arg4))

(defun +gptel-ediff-buffers (old-buffer new-buffer &optional startup-hooks)
  "Launch ediff between OLD-BUFFER and NEW-BUFFER with proper cleanup.

OLD-BUFFER is the buffer containing the original content.
NEW-BUFFER is the buffer containing the proposed changes.
STARTUP-HOOKS are optional hooks to run after ediff setup.

This function:
1. Sets up ediff with plain window configuration
2. Ensures proper window selection (avoids side windows)
3. Automatically cleans up the NEW-BUFFER after ediff completion
4. Restores the original window configuration

Returns the ediff session buffer.

(fn OLD-BUFFER NEW-BUFFER &optional STARTUP-HOOKS)"
  (let ((orig-window (selected-window))
	(orig-window-config (current-window-configuration))
	(ediff-window-setup-function #'ediff-setup-windows-plain))
    ;; make sure the current window is not a not side window
    (when (window-parameter orig-window 'window-side)
      (select-window (get-window-with-predicate
		      (lambda (win)
			(null (window-parameter win 'window-side))))))
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

(eval-when-compile (require 'llama))

(defun +gptel--popup-1 (prompt actions)
  "Display PROMPT and allow user to choose between one of several ACTIONS.
ACTIONS is a list of lists (KEY DESC FUNC ARGS...).  KEY is a string
identifying the key that triggers this action; it is passed to
`key-parse'.  DESC is a description string to be displayed in the popup.
If it is nil, the action and its binding is not displayed in the popup,
although it still takes effect.  If the user selects an action, its FUNC
is called with ARGS and popup is dismissed.  The return value of
`+gptel-popup' is the return value of FUNC."
  (let ((keymap (make-sparse-keymap))
	(prompt (concat prompt "\n"))
	(max-key-lenght (seq-reduce
			 (##max %1 (length (car %2)))
			 actions 0))
	func)
    (pcase-dolist (`(,key ,desc ,func . ,args) actions)
      (when desc
	(setq prompt
	      (concat prompt
		      (format " %s%s  %s\n"
			      key
			      (make-string (- max-key-lenght (length key)) ?\s)
			      desc))))
      (keymap-set keymap key (lambda ()
			       (interactive)
			       (apply func args))))
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

(defmacro +gptel--popup (prompt actions)
  "Create a popup with PROMPT and ACTIONS.
ACTIONS is a list of (KEY DESC BODY...) forms, where KEY is the keybinding,
DESC is the description, and BODY is the code to execute when KEY is selected."
  `(+gptel--popup-1
    ,prompt
    (list ,@(mapcar
	     (lambda (action)
	       (pcase-let ((`(,key ,desc . ,body) action))
		 `(list ,key ,desc (lambda () ,@body))))
	     actions))))


(defun +gptel-ediff-file-with-buffer (filename buffer callback &optional startup-hooks)
  "Launch ediff between FILENAME and BUFFER with interactive approval.

FILENAME is the path to the file to compare against.
BUFFER is the buffer containing proposed changes.
CALLBACK is a function called with the result message.
STARTUP-HOOKS are optional hooks to run after ediff setup.

This function:
1. Opens the file and compares it with the buffer content using ediff
2. Presents the user with options to accept, reject, or request changes
3. Writes the changes to the file if accepted
4. Calls CALLBACK with appropriate result message
5. Handles gptel session cleanup on rejection

Returns the ediff session buffer.

(fn FILENAME BUFFER CALLBACK &optional STARTUP-HOOKS)"
  (let* ((session-buffer (and (bound-and-true-p gptel-mode)
			      (current-buffer)))
	 (file-buffer (find-file-noselect filename))
	 (quit-func (lambda ()
		      (+gptel--popup
		       "Accept changes? "
		       (("a" "Apply all proposed changes"
			 (with-current-buffer buffer
			   (write-region nil nil filename))
			 (funcall callback "Successfully edited file."))
			("q" "Quit and keep the file as is"
			 (with-current-buffer file-buffer
			   (save-buffer))
			 (funcall callback "Not all edits applied.  You MUST reread the file to see what has changed."))
			("k" "Reject the changes"
			 (funcall callback "User rejected the changes.")
			 (when session-buffer
			   (gptel-abort session-buffer)))
			("r" "Iterate the changes"
			 (funcall callback
				  (concat "User requested changes to your edits: "
					  (read-string "Request changes: "))))))))
	 (startup-func (lambda ()
			 (add-hook 'ediff-quit-hook quit-func nil t))))
    (+gptel-ediff-buffers file-buffer
			  buffer
			  (cons startup-func startup-hooks))))

(defun +gptel-string-replace-async (callback file-path old-string new-string)
  (cl-assert gptel-mode)
  (cl-assert (stringp file-path))
  (cl-assert (not (string-empty-p old-string)))
  (condition-case-unless-debug err
      (let ((edit-buffer (generate-new-buffer "*edit-file*")))
	(with-current-buffer edit-buffer
	  (insert-file-contents (expand-file-name file-path))
	  (let* ((inhibit-read-only t)
		 (case-fold-search nil)
		 (ediff-window-setup-function 'ediff-setup-windows-plain))
	    (goto-char (point-min))
	    (unless (search-forward old-string nil t)
	      (error "Failed to find the string to replace"))
	    (save-match-data
	      (when (search-forward old-string nil t)
		(error "Failed to replace: the `old_string' is not unique.")))
	    (replace-match new-string t t)))
	(+gptel-ediff-file-with-buffer file-path edit-buffer callback))
    (error (funcall callback (format "An error occurred: %S" err))
	   (signal (car err) (cdr err)))))

(gptel-make-tool
 :name "str_replace"
 :function #'+gptel-string-replace-async
 :async t
 :description "Replace a specific string in a file with a new string.

You must use `read_file` to get the file's exact contents before editing.
The `old_string` must match the original content exactly, including all
indentation and newlines. The edit will fail if `old_string` is not unique.
Prefer small, targeted edits over large replacements."
 :args (list '(:name "file_path"
		     :type string
		     :description "The full path of the file to modify")
	     '(:name "old_string"
		     :type string
		     :description "The text to replace (MUST match exactly, including whitespaces and indentation)"
		     )
	     '(:name "new_string"
		     :type string
		     :description "The new text to insert in place of the old text"))
 :category "filesystem")

(defun +gptel-insert-async (callback file-path insert-line new-string)
  (cl-assert (stringp file-path))
  (cl-assert (natnump insert-line))
  (cl-assert (stringp new-string))
  (condition-case-unless-debug err
      (let ((edit-buffer (generate-new-buffer "*edit-file*")))
	(with-current-buffer edit-buffer
	  (insert-file-contents file-path)
	  (goto-char (point-min))
	  (forward-line insert-line)
	  (insert new-string ?\n))
	(+gptel-ediff-file-with-buffer file-path edit-buffer callback))
    (error (funcall callback (format "An error occurred: %S" err))
	   (message "Error: %S" err))))

(gptel-make-tool
 :name "insert"
 :function #'+gptel-insert-async
 :async t
 :description "Insert text at a specific location in a file."
 :args (list (list :name "file_path"
		   :type 'string
		   :description "The full path of the file to modify")
	     (list :name "insert_line"
		   :type 'number
		   :description "The line number after which to insert the text (0 for beginning of file)")
	     (list :name "new_string"
		   :type 'string
		   :description "The text to insert"))
 :category "filesystem")

(gptel-make-tool
 :name "grep"
 :function
 (lambda (callback regexp working-dir)
   (condition-case err
       (let* ((default-directory (or (and working-dir (expand-file-name working-dir))
				     default-directory))
	      (compilation-buffer-name-function #'project-prefixed-buffer-name)
	      (buffer (grep (format "rg  --no-heading -n -e %s | head -n 200" (shell-quote-argument regexp)))))
	 (with-current-buffer buffer
	   (add-hook 'compilation-finish-functions
		     (lambda (buffer _how)
		       (with-current-buffer buffer
			 (funcall callback (buffer-substring-no-properties
					    (point-min) (point-max)))))
		     nil t)))
     (error (funcall callback (list :error "Failed to run grep"
				    :internal-error err)))))
 :async t
 :description "Search for a pattern in the workspace.
Supports full regex syntax (eg. \"log.*Error\", \"function|var\\s+\\w+\", etc). "
 :args (list '(:name "regexp"
		     :type string
		     :description "The pattern to search.  It should be a regular expression.")
	     '(:name "working_dir"
		     :type string
		     :description "Optional: The directory in which to run the search. Defaults to the current directory if not specified."))
 :category "filesystem")

;; command tools


(defun +gptel-run-command-async (callback command working-dir)
  (let* ((default-directory (expand-file-name working-dir))
	 (command-buffer-name (if (project-current)
				  (concat " *" (project-name (project-current)) " : Command Output*")
				" *Command Output*"))
	 (buffer (get-buffer-create command-buffer-name))
	 proc start-marker)
    (with-current-buffer buffer
      (unless (derived-mode-p 'comint-mode)
	(comint-mode)
	(setq-local process-environment (append (list "PAGER=cat"
						      "GIT_PAGER=cat")
						process-environment)))
      (goto-char (point-max))
      ;; Insert separator and command before running
      (insert "\n" (make-string 60 ?-) "\n")
      (insert-before-markers (propertize "$ " 'face 'comint-highlight-prompt) command "\n")
      (comint-add-to-input-history command)
      (setq start-marker (point-marker)))
    (display-buffer buffer)
    (with-editor
      (comint-exec buffer
		   "gptel-run-command"
		   shell-file-name
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

(gptel-make-tool
 :name "run_command"
 :function #'+gptel-run-command-async
 :async t
 :description "Executes a shell command and returns the output as a string.

- IMPORTANT: You MUST avoid using search command `grep`.  Instead use the
`grep` tool to search.  You MUST avoid read tools like `cat`, `head`,
`tail`, and `ls`, and use `read_file` and `list_directory` tool.  (But it's
ok to pipe long outputs to `head` to avoid reading to much).

- It is very helpful if you write a clear, concise description of what
this command does in 5-10 words, before calling the tool.

- When issuing multiple commands, use the ';' or '&&' operator to separate them.  DO NOT use newlines (newlines are ok in quoted strings)

- Try to maintain your current working directory throughout the session by
using absolute paths and avoiding usage of `cd`.  You may use `cd` if the User
explicitly requests it.

  <good-example>
  pytest /foo/bar/tests
  </good-example>

  <bad-example>
  cd /foo/bar && pytest ./test
  </bad-example>

- When running commands that support auto-paging, add --no-pager if applicable.

## Committing changes with git

When the user asks you to create a new git commit, following these steps
carefully:

1.:
    - Run a git status command to see all untrack files.
    - Run a git diff command to see both staged and unstaged changes that will be commited.
    - Run a git log command to see recent commit messages, so that you can follow this repository's commit message style.  (Don't forget to pipe the result to head to limit the count of commits)

2.: Analyze all staged changes (both previously staged and newly added) and draft a
commit message.
Wrap your analysis process in <commit_analysis> tags:

<commit_analysis>
- List the files that have been changed or added
- Summarize the nature of the changes (eg. new feature, enhancement to an existing feature, bug fix, refactoring, test, docs, etc.)
- Brainstorm the purpose or motivation behind these changes
- Assess the impact of these changes on the overall project
- Check for any sensitive information that shouldn't be committed
- Draft a concise (1-2 sentences) commit message that focuses on the \"why\" rather than the \"what\"
- Ensure your language is clear, concise, and to the point
- Ensure the message accurately reflects the changes and their purpose (i.e. \"add\" means a wholly new feature, \" update \" means an enhancement to an existing feature, \"fix\" means a bug fix, etc.)
- Ensure the message is not generic (avoid words like \"Update\" or \"Fix\" without context)
- Review the draft message to ensure it accurately reflects the changes and their purpose
</commit_analysis>

4.  If the commit fails due to pre-commit hook changes, retry the commit ONCE to include these automated changes. If it fails again, it usually means a pre-commit hook is preventing the commit. If the commit succeeds but you notice that files were modified by the pre-commit hook, you MUST amend your commit to include them.

Important notes:
- Use the git context at the start of this conversation to determine which files are relevant to your commit. Be careful not to stage and commit files (e.g. with `git add .`) that aren't relevant to your commit.
- NEVER update the git config
- DO NOT run additional commands to read or explore code, beyond what is available in the git context
- DO NOT push to the remote repository
- IMPORTANT: Never use git commands with the -i flag (like git rebase -i or git add -i) since they require interactive input which is not supported.
- If there are no changes to commit (i.e., no untracked files and no modifications), do not create an empty commit
- Ensure your commit message is meaningful and concise. It should explain the purpose of the changes, not just describe them.
- Return an empty response - the user will see the git output directly
- In order to ensure good formatting, ALWAYS pass the commit message via a HEREDOC, a la this example:
<example>
git commit -m \"$(cat <<'EOF'
   Commit message here.
   EOF
   )\"
</example>
"
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

;; Emacs tools

(defun +gptel--flymake-diag-to-json (diag)
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
	  :end :end
	  :type type
	  :origin origin
	  :code code
	  :message message
	  :backend backend)))

(gptel-make-tool
 :name "editor_diagnostics"
 :description "Get editor diagnostics (errors, warnings, info) for workspaces.
Returns project-wide diagnostics when no file path is provided, or file-specific
diagnostics when a file path is given. Requires the file to be open in the editor."
 :function (lambda (file-path)
	     (require 'flymake)
	     (if (or (null file-path) (string-empty-p file-path))
		 (mapcar #'+gptel--flymake-diag-to-json (flymake--project-diagnostics))
	       (if-let* ((buffer (get-file-buffer file-path)))
		   (with-current-buffer buffer
		     (mapcar #'+gptel--flymake-diag-to-json (flymake-diagnostics)))
		 (error "File not opened in editor: %s" file-path))))
 :args (list '(:name "file_path"
		     :type string
		     :description "Optional file path to get specific file diagnostics. Leave empty for project-wide diagnostics."))
 :category "emacs")

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

(defmacro +gptel-with-url-retrieve (callback url &rest body)
  (declare (indent 2))
  (let ((url-var (gensym "url-"))
	(callback-var (gensym "cb-"))
	(status-var (gensym "status-"))
	(err-var (gensym "err-"))
	(err-var1 (gensym "err-")))
    `(let ((,url-var ,url)
	   (,callback-var ,callback))
       (condition-case ,err-var1
	   (url-retrieve
	    ,url-var
	    (lambda (,status-var)
	      (condition-case ,err-var
		  (progn
		    (message "Retrieving %s...%s" ,url-var url-http-response-status)
		    (if (>= url-http-response-status 400)
			(error "http error %s: %s"
			       url-http-response-status
			       (buffer-string))
		      (funcall callback
			       (progn ,@body))))
		(error (funcall ,callback-var (list :error "Failed to fetch the URL"
						    :internal-error ,err-var))))))
	 (error (funcall ,callback-var (list :error "Failed to fetch the URL"
					     :internal-error ,err-var1)))))))

(gptel-make-tool
 :name "read_url"
 :function
 (lambda (callback url)
   (+gptel-with-url-retrieve callback url
     (goto-char (point-min))
     (forward-paragraph)
     (let ((dom (libxml-parse-html-region (point) (point-max))))
       (run-at-time 0 nil #'kill-buffer (current-buffer))
       (with-temp-buffer
	 (shr-insert-document dom)
	 (buffer-substring-no-properties (point-min) (point-max))))))
 :async t
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
(defun +gptel-search-ddg-async (callback query)
  (let ((url (format "https://html.duckduckgo.com/html/?q=%s" query)))
    (+gptel-with-url-retrieve callback url
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
 :function #'+gptel-search-ddg-async
 :async t
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
 :category "internal")

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

(define-advice forge--post-submit-errorback (:before (&rest _args) +gptel-hook)
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

;;; Todo List Management

(defvar +gptel-todo-file nil
  "File to store todo items in Org format.")

(defun +gptel-get-project-todo-file ()
  "Get the project-specific todo file path."
  (let ((project-root (or (locate-dominating-file default-directory ".git")
                          (locate-dominating-file default-directory "AGENTS.md")
                          default-directory)))
    (expand-file-name ".cache/todo.org" project-root)))

(defun +gptel-ensure-todo-directory ()
  "Ensure the .cache directory exists for the todo file."
  (let ((todo-file (+gptel-get-project-todo-file)))
    (unless (file-directory-p (file-name-directory todo-file))
      (make-directory (file-name-directory todo-file) t))
    todo-file))

(defun +gptel-read-todo-file ()
  "Read todo items from the project-specific todo file."
  (let ((todo-file (+gptel-ensure-todo-directory)))
    (if (file-exists-p todo-file)
        (with-temp-buffer
          (insert-file-contents todo-file)
          (buffer-string))
      "No todo file found for this project. Create one by adding your first todo item.")))

(gptel-make-tool
 :name "read_todos"
 :function #'+gptel-read-todo-file
 :description "Read all todo items from the project-specific todo list file"
 :category "todo")

(defun +gptel-add-todo-item (title &optional description priority tags)
  "Add a new todo item to the project-specific todo file."
  (let* ((todo-file (+gptel-ensure-todo-directory))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (priority-str (if priority (format "[#%s] " priority) ""))
         (tags-str (if tags (format ":%s:" tags) "")))
    (with-temp-buffer
      (when (file-exists-p todo-file)
        (insert-file-contents todo-file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* TODO " priority-str title " " tags-str "\n")
      (insert "  :PROPERTIES:\n")
      (insert "  :CREATED: " timestamp "\n")
      (insert "  :END:\n")
      (when description
        (insert "  " description "\n"))
      (write-region (point-min) (point-max) todo-file))
    (format "Added todo item: %s" title)))

(gptel-make-tool
 :name "add_todo"
 :function #'+gptel-add-todo-item
 :description "Add a new todo item to the project-specific todo list"
 :args (list '(:name "title"
		     :type string
		     :description "The title of the todo item")
	     '(:name "description"
		     :type string
		     :description "Optional description of the todo item")
	     '(:name "priority"
		     :type string
		     :description "Optional priority (A, B, C)")
	     '(:name "tags"
		     :type string
		     :description "Optional tags separated by colons"))
 :category "todo")

(defun +gptel-mark-todo-done (title-or-pattern)
  "Mark a todo item as DONE by title or pattern in the project-specific file."
  (let ((todo-file (+gptel-ensure-todo-directory)))
    (if (not (file-exists-p todo-file))
        "No todo file found for this project."
      (let ((found nil)
            (case-fold-search t))
        (with-temp-buffer
          (insert-file-contents todo-file)
          (goto-char (point-min))
          (while (re-search-forward (concat "^\\* \\(?1:TODO\\) \\(?:\\[#.\\] \\)?\\(.*\\)" (regexp-quote title-or-pattern) "\\(.*\\)") nil t)
            (replace-match "DONE" t nil nil 1)
            (setq found t))
          (when found
            (write-region (point-min) (point-max) todo-file)))
        (if found
            (format "Marked todo item as DONE: %s" title-or-pattern)
          (format "No todo item found matching: %s" title-or-pattern))))))

(gptel-make-tool
 :name "complete_todo"
 :function #'+gptel-mark-todo-done
 :description "Mark a todo item as completed/DONE in the project-specific list"
 :args (list '(:name "title_or_pattern"
		     :type string
		     :description "The title or pattern to match the todo item to complete"))
 :category "todo")

(defun +gptel-search-todos (pattern)
  "Search for todo items matching a pattern in the project-specific file."
  (let ((todo-file (+gptel-ensure-todo-directory)))
    (if (not (file-exists-p todo-file))
        "No todo file found for this project."
      (let ((matches nil))
        (with-temp-buffer
          (insert-file-contents todo-file)
          (goto-char (point-min))
          (while (re-search-forward (concat "^\\* " (regexp-quote pattern)) nil t)
            (beginning-of-line)
            (let ((line (buffer-substring-no-properties (point) (line-end-position))))
              (push line matches))
            (forward-line 1)))
        (if matches
            (string-join (reverse matches) "\n")
          (format "No todo items found matching: %s" pattern))))))

(gptel-make-tool
 :name "search_todos"
 :function #'+gptel-search-todos
 :description "Search for todo items matching a pattern in the project-specific list"
 :args (list '(:name "pattern"
		     :type string
		     :description "The search pattern to match todo items"))
 :category "todo")

(defun +gptel-list-active-todos ()
  "List all active (TODO) items in the project-specific file."
  (let ((todo-file (+gptel-ensure-todo-directory)))
    (if (not (file-exists-p todo-file))
        "No todo file found for this project."
      (let ((todos nil))
        (with-temp-buffer
          (insert-file-contents todo-file)
          (goto-char (point-min))
          (while (re-search-forward "^\\* TODO " nil t)
            (beginning-of-line)
            (let ((line (buffer-substring-no-properties (point) (line-end-position))))
              (push line todos))
            (forward-line 1)))
        (if todos
            (string-join (reverse todos) "\n")
          "No active todo items found for this project.")))))

(gptel-make-tool
 :name "list_active_todos"
 :function #'+gptel-list-active-todos
 :description "List all active (TODO) items from the project-specific todo list"
 :category "todo")

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
  :temperature 1.3
  :stream t)

(gptel-make-preset 'deepseek-assistant
  :description "General assistant powered by DeepSeek"
  :system "You are an LLM lived in Emacs and a helpful assistant.
Be concise, accurate, and helpful.
You may search the web or read URLs when needed.
Whenever you cite external information, always include the full source URL."
  :backend "DeepSeek"
  :model 'deepseek-chat
  :stream t
  :temperature 1.3
  :use-tools t
  :max-tokens 4096
  :include-tool-results t
  :tools '("search_web" "read_url" "read_documentation" "search_emacs_mailing_list"))

(gptel-make-preset 'deepseek-mathematician
  :description "Expert mathematics assistant using DeepSeek"
  :backend "DeepSeek"
  :model 'deepseek-reasoner
  :stream t
  :temperature 0.1
  :max-tokens 4096
  :use-tools nil
  :system "You are an expert mathematician and problem solver. Provide accurate
mathematical solutions, proofs, and explanations.
Output math formulas in Latex format.
")


(gptel-make-preset 'kimi-assistant
  :description "Kimi with web search and URL reading"
  :backend "Moonshot"
  :model 'kimi-k2-turbo-preview
  :stream t
  :temperature 0.6
  :use-tools 'force
  :tools '("search_web" "read_url" "read_documentation" "search_emacs_mailing_list")
  :system "You are Kimi, an Emacs-embedded LLM assistant.
Be concise, accurate, and helpful.
You may search the web or read URLs when needed.
Whenever you cite external information, always include the full source URL.")

(gptel-make-preset 'coding-agent
  :description "Fast, deterministic coding assistant using file operations and project management"
  :backend "Moonshot"
  :model 'kimi-k2-turbo-preview
  :stream t
  :temperature 0.1
  :max-tokens 8192
  :use-tools t
  :tools '("str_replace" "insert" "create_file" "write_file" "read_file"
	   "run_command" "grep" "list_directory" "read_todos" "add_todo"
	   "complete_todo" "search_todos" "list_active_todos"
	   "editor_diagnostics")
  :system "You are ECA (Emacs Coding Agent), an AI coding agent that operates in Emacs.

You are pair programming with a USER to solve their coding task.  Each
time the USER sends a message, we may automatically attach some context
information about their current state, such as passed contexts, rules
defined by USER, project structure, and more.  This information may or
may not be relevant to the coding task -- it is up to you to decide.

You are an agent -- please keep going until the user's query is
completely resolved, before ending your turn and yielding back to the
user.  Only terminate your turn when you are sure that the problem is
solved.  Autonomously resolve the query to the best of you ability
before coming back to the user.

NEVER show the code edits or new files to the user -- only call the
proper tool.  The system will apply and display the edits.  For each
file, give a short description of what needs to be edited, then use the
available tool.  You can use the tool multiple times in a response, and
you can keep writing text after using a tool.  Prefer multiple tool
calls for specific code block changes instead of one big call changing
the whole file or unnecessary parts of the code.

Use proper Markdown formatting in your answer.  When referring to a
filename, function, symbol in the user's workspace, wrap it in
backticks.  Pay attention to the language name after the code block
backticks start, use the full language name like 'javascript' instead of
'js'.

You have tools at your disposal to solve the coding task.  Follow these
rules regarding tool calls:

1. ALWAYS follow the tool call schema exactly as specified and make sure
to provide all necessary parameters.

2. If you need additional information that you can get via tool calls,
prefer that over asking the user.

3. If you are not sure about file content or codebase structure
pertaining to the user's request, use your tools to read files and gather
the relevant information: do NOT guess or make up an answer.

4. You have the capability to call multiple tools in a single response,
batch your tool calls together for optimal performance.

5. You should prefer the dedicated tools over the generic run_command
tool.

IMPORTANT: When starting a new session, first check if an AGENTS.md file
exists in the project root directory. If it does, read it to understand
the project-specific development guidelines and conventions that should
be followed during the coding session.

For multi-step tasks, maintain a project-specific todo list using the
todo management tools. Before starting work:
1. Check existing todos with read_todos
2. Add new todo items for each step with add_todo
3. Mark completed steps with complete_todo
4. Search for specific todos when needed with search_todos

This helps track progress and ensures no steps are missed in complex tasks.")

(gptel-make-preset 'deepseek-reasoner
  :description "DeepSeek Reasoner – step-by-step reasoning assistant"
  :backend "DeepSeek"
  :model 'deepseek-reasoner
  :stream t
  :temperature 0.3
  :max-tokens 8192
  :use-tools nil
  :system "You are DeepSeek Reasoner. Think step-by-step, expose your
chain-of-thought, and verify every conclusion before presenting the
final answer.")

(gptel-make-preset 'kagi-search
  :description "Kagi search assistant"
  :backend "Kagi"
  :stream t
  :temperature 0.5
  :use-tools nil
  :system "You are a search assistant powered by Kagi. Provide accurate, concise
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
			     (file-local-name (expand-file-name filename)) "--exclude=SC1091,SC2034")
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
    (setq-local flymake-show-diagnostics-at-end-of-line 'fancy)
    (flymake-mode 1))
  (let* ((short-name (file-name-nondirectory file))
	 (gpt-buf (gptel (format "*ShellCheck/%s*" short-name))))
    (pop-to-buffer gpt-buf)
    (with-current-buffer gpt-buf
      (setq-local gptel-backend (gptel-get-backend "Copilot"))
      (setq-local gptel-model 'claude-3.7-sonnet)
      (setq-local gptel-tools
		  (append (gptel-get-tool "filesystem")
			  (ensure-list (gptel-get-tool "search_web"))
			  (ensure-list (gptel-get-tool "read_url"))
			  (ensure-list (gptel-get-tool "shellcheck"))))
      (setq-local gptel-confirm-tool-calls nil)
      (setq-local gptel-use-tools t)
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
5. Use the `str_replace' tool to edit the script
"
		      file))
      (let ((gptel-use-tools 'force))
	(gptel-send)))))


(defvar gptel-agent-backend gptel-backend)
(defvar gptel-agent-model gptel-model)

;;;###autoload
(defun gptel-agent ()
  "Create or switch to a gptel session buffer for the current project.
The buffer will be displayed in a side window by default, but this behavior
can be customized.
The session buffer will use the coding-agent preset.  The default-directory in
the session buffer will be the project root if there is a project (as found
by project-current in project.el), otherwise the default-directory where the
command is invoked."
  (interactive)
  (let* ((project-root (when-let* ((proj (project-current)))
			 (if (fboundp 'project-root)
			     (project-root proj)
			   ;; Suppress warning about obsolete project-roots function
			   (with-suppressed-warnings ((obsolete project-roots))
			     (car (project-roots proj))))))
	 (proj-name (and project-root
			 (project-name (project-current))))
	 (buffer-name (if proj-name
			  (format "*gptel-agent : %s*" proj-name)
			"*gptel-agent*"))
	 (existing-buffer-p (get-buffer buffer-name)))
    (gptel buffer-name)
    (pop-to-buffer buffer-name '((display-buffer-in-side-window)
				 (side . right)))
    (with-current-buffer (get-buffer buffer-name)
      (unless existing-buffer-p
	;; Apply coding-agent preset settings
	(gptel--apply-preset 'coding-agent
			     (lambda (sym val)
			       (set (make-local-variable sym) val)))
	;; Set local variable to include tool results in buffer
	(setq-local gptel-include-tool-results t)
	(setq-local gptel-backend gptel-agent-backend
		    gptel-model gptel-agent-model))
      (when project-root
	(cd project-root))
      (gptel-agent--setup-context)
      (unless existing-buffer-p
	(when project-root
	  (insert (format "I'm working in project: %s\n" project-root)))
	(message "gptel-agent session started with coding-agent preset")))))

(defun gptel-agent--setup-context ()
  (dolist (filename (list "AGENTS.md"))
    (let ((filename (expand-file-name filename)))
      (when (file-exists-p filename)
	(gptel-add-file filename)))))

(provide 'gptel-config)
;;; gptel-config.el ends here
