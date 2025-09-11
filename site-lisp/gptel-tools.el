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

;; This package provides a collection of agentic tools for gptel, enabling
;; interactive file operations, code editing, and system interactions through
;; AI-assisted workflows. The tools are designed to work seamlessly with
;; gptel-mode, providing both synchronous and asynchronous operations with
;; proper user confirmation and error handling.

;; Key features include:
;; - File reading, writing, and creation with user preview
;; - Interactive ediff-based file comparison and editing
;; - String replacement and text insertion operations
;; - Directory listing and file system navigation
;; - Shell command execution with output capture
;; - Code diagnostics via Flymake integration
;; - Web content retrieval and search capabilities
;; - Git integration and change management workflows

;; All tools are registered with gptel using `gptel-make-tool` and include
;; comprehensive documentation, argument specifications, and proper error
;; handling. Asynchronous operations use callback patterns to maintain
;; responsiveness during long-running tasks.

;;; Code:

(require 'gptel)
(require 'with-editor)
(require 'flymake)
(eval-when-compile
  (require 'seq)
  (require 'llama))

;;;; tools

(defun gptel-tools---popup-1 (prompt actions)
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

(defmacro gptel-tools---popup (prompt actions)
  "Create a popup with PROMPT and ACTIONS.
ACTIONS is a sequence of (KEY DESC BODY...) forms, where KEY is the
keybinding, DESC is the description, and BODY is the code to execute
when KEY is selected.

See also `gptel-tools--popup-1'."
  `(gptel-tools---popup-1
    ,prompt
    (list ,@(mapcar
	     (lambda (action)
	       (pcase-let ((`(,key ,desc . ,body) action))
		 `(list ,key ,desc (lambda () ,@body))))
	     actions))))

(defvar ediff-window-setup-function)

(declare-function ediff-setup-windows-plain "ediff-wind.el" (arg1 arg2 arg3 arg4))

(defun gptel-tools--ediff-buffers (old-buffer new-buffer &optional startup-hooks)
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
				(concat  "User rejected the changes: "
					 (read-string "Requested changes: ")))
		       (when quit
			 (ediff-quit nil))))
	 (quit-func (lambda ()
		      (when callback
			(gptel-tools---popup-1
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

FILEPATH is the path to the file to read. Supports relative paths and ~.
START is the first line to read (Start from 1).
END is the last line of the file to read. Can be -1 to read to end of file.

Returns the file contents as a string."
  (unless (file-exists-p filepath)
    (error "File not found: %s" filepath))
  (let ((file-buffer (find-file-noselect filepath)))
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

(defun gptel-tools--list-directory (directory)
  "List the contents of a given directory.

DIRECTORY is the path to the directory to list.

Returns a string containing the directory contents separated by newlines."
  (mapconcat #'identity
	     (directory-files directory)
	     "\n"))

(defun gptel-tools--create-file (path filename content)
  "Create a new file with the specified content.

PATH is the directory where to create the file.
FILENAME is the name of the file to create.
CONTENT is the content to write to the file.

Returns a success message or raises an error if file already exists."
  (let ((full-path (expand-file-name filename path)))
    (if (file-exists-p full-path)
	(error "File %s already exists" filename)
      (with-temp-buffer
	(insert content)
	(write-file full-path))
      (format "Created file %s in %s" filename path))))

(defun gptel-tools--write-file-async (callback file-path content)
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
  (let* ((preview-buffer (generate-new-buffer "*write-file-preview*")))
    (condition-case err
	(with-current-buffer preview-buffer
	  (insert content))
      (:success (gptel-tools--ediff-file-with-buffer file-path preview-buffer callback))
      (error (funcall callback (format "An error occurred: %S" err))))))


(defun gptel-tools--insert-async (callback file-path insert-line new-string)
  (cl-assert (stringp file-path))
  (cl-assert (natnump insert-line))
  (cl-assert (stringp new-string))
  (let ((edit-buffer (generate-new-buffer "*edit-file*")))
    (condition-case-unless-debug err
	(with-current-buffer edit-buffer
	  (insert-file-contents file-path)
	  (goto-char (point-min))
	  (unless (zerop (forward-line insert-line))
	    (error "line number out of range"))
	  (insert new-string ?\n))
      (:success (gptel-tools--ediff-file-with-buffer file-path edit-buffer callback))
      (error (funcall callback (format "An error occurred: %S" err))))))

(defun gptel-tools--string-replace-async (callback file-path old-string new-string)
  "Asynchronously replace OLD-STRING with NEW-STRING in FILE-PATH.
This function performs a string replacement operation in the specified file
and uses ediff to present the changes for user confirmation.

CALLBACK is a function called with the result of the operation.
FILE-PATH is the path to the file to modify.
OLD-STRING is the string to search for and replace.
NEW-STRING is the replacement string.

The function will error if:
- gptel-mode is not active
- FILE-PATH is not a valid string
- OLD-STRING is empty
- OLD-STRING is not found in the file
- OLD-STRING appears multiple times in the file (not unique)

After replacement, an ediff session is started for user confirmation.
The CALLBACK function receives either a success message or an error string."
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
      (:success (gptel-tools--ediff-file-with-buffer file-path edit-buffer callback))
      (error (funcall callback (format "An error occurred: %S" err))
	     (kill-buffer edit-buffer)))))

(defun gptel-tools--grep-async (callback regexp working-dir)
  "Asynchronously search for REGEXP in WORKING-DIR using ripgrep.

This function runs ripgrep (rg) asynchronously to search for REGEXP
in the specified WORKING-DIR (or current directory if nil). The
search results are limited to the first 200 matches. When the
search completes, CALLBACK is invoked with the results as a string.

CALLBACK is a function that takes a single string argument containing
the search results. If an error occurs during the search, CALLBACK
is called with a list containing error information.

REGEXP is the regular expression pattern to search for.
WORKING-DIR is the directory to search in (nil means current directory)."
  (condition-case err
      (let* ((default-directory (or (and working-dir (expand-file-name working-dir))
				    default-directory))
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
				   :internal-error err)))))


(defun gptel-tools--shell-command-async (callback command working-dir)
  "Execute COMMAND asynchronously in WORKING-DIR and call CALLBACK with output.

COMMAND is a shell command string to execute.
WORKING-DIR is the directory in which to run the command, or nil for default.
CALLBACK is a function called with the command output string when complete.

The command runs in a dedicated buffer with comint-mode, and the output
is captured from after the command display marker. The callback receives
the command output as a string, or nil if the buffer is no longer alive."
  (let* ((default-directory (if working-dir
				(expand-file-name working-dir)
			      default-directory))
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
    (if-let* ((buffer (get-file-buffer file-path)))
	(with-current-buffer buffer
	  (mapcar #'gptel-tools--flymake-diag-to-json (flymake-diagnostics)))
      (error "File not opened in editor: %s" file-path))))


(defvar url-http-response-status)

(defun gptel-tools--read-url-async (callback url)
  "Asynchronously retrieve and parse URL content.

CALLBACK is a function called with either:
- The parsed content string on success
- A plist (:error \"message\" :internal-error ERROR-OBJ) on failure

URL is the URL string to retrieve.

The function uses `url-retrieve' to fetch the content asynchronously,
parses the HTML using `libxml-parse-html-region', and renders it to
text using `shr-insert-document'."
  (condition-case err1
      (url-retrieve
       url
       (lambda (_status)
	 (condition-case err
	     (funcall callback
		      (progn
			(goto-char (point-min))
			(forward-paragraph)
			(let ((dom (unwind-protect
				       (libxml-parse-html-region (point) (point-max))
				     (kill-buffer (current-buffer)))))
			  (with-temp-buffer
			    (shr-insert-document dom)
			    (buffer-substring-no-properties (point-min) (point-max))))))
	   (error (funcall callback (list :error "Failed to read url"
					  :internal-error err))))))
    (error (funcall callback (list :error "Failed to read url"
				   :internal-error err1)))))

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
  "Asynchronously search the web using DuckDuckGo.

CALLBACK is a function to call with the results. It will be called
with either a string containing the formatted search results on success,
or a list of the form (:error MESSAGE :internal-error ERROR) on failure.

QUERY is the search query string.

This function uses `url-retrieve' to fetch the HTML results from
DuckDuckGo, parses the response with `libxml-parse-html-region',
and formats it using `shr-insert-document' with custom rendering
for links via `gptel-tools--insert-link-strip-href'."
  (condition-case err1
      (url-retrieve
       (format "https://html.duckduckgo.com/html/?q=%s" query)
       (lambda (_status)
	 (condition-case err
	     (if (>= url-http-response-status 400)
		 (error "http error %s: %s"
			url-http-response-status
			(buffer-string))
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
			       (point-min) (point-max)))))))
	   (error
	    (funcall callback
		     (list :error
			   "Failed to fetch the URL"
			   :internal-error err))))))
    (error
     (funcall callback
	      (list :error "Failed to fetch the URL" :internal-error
		    err1)))))


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
 :args (list '(:name "file_path"
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
 :args (list (list :name "file_path"
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
 :args (list '(:name "file_path"
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
 :confirm t)

(gptel-make-tool
 :name "editor_diagnostics"
 :description "Get editor diagnostics (errors, warnings, info) for workspaces.
Returns project-wide diagnostics when no file path is provided, or file-specific
diagnostics when a file path is given. Requires the file to be open in the editor."
 :function #'gptel-tools--editor-diagnostics
 :args (list '(:name "file_path"
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

(provide 'gptel-tools)
;;; gptel-tools.el ends here
