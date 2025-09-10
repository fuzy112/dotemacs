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
(require 'gptel-transient)
(require 'gptel-tools)
(require 'url-http)
(require 'with-editor)
(require 'flymake)
(require 'dom)

;;; Reset backends, tools, and presets

(setq gptel--known-backends nil
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
	     :input-cost 1.50	  ; when cache hit
					; when cache misses:
					;  0.20 when (context < 8k)
					;  1.00 when (8k  <= context < 32k)
					;  2.00 when (32k <= context < 128k)
	     :output-cost 2.00)	      	; (context < 8k)
					; 3.00  hen (8k <= context < 32k)
					; 5.00 when (32k <= context < 127k)
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
	     :input-cost 0.15		; 0.60 if cache miss
	     :output-cost 2.50)
	    (kimi-k2-0905-preview
	     :description "A model suitable for coding"
	     :capabilities (media tool-use json)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 256
	     :input-cost 0.15		; 0.60 if cache miss
	     :output-cost 2.50)
	    (kimi-k2-turbo-preview
	     :description "A model suitable for coding"
	     :capabilities (media tool-use json)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 256
	     :input-cost 0.60		; 2.50 if cache miss
	     :output-cost 10.00)
	    (moonshot-v1-auto
	     :description "The standard Moonshot V1 model"
	     :capabilities (tool-use json)
	     :context-window 128
	     :input-cost 0.20	  ; when (context < 8k)
					; 1.00 when (8k <= context < 32k)
					; 2.00 when (32k  <= context < 128k)
	     :output-cost 2.00))	; when (context < 8k)
					; 3.00 when (8k  <= context < 32k)
					; 500  when (32k <= context < 128k)
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

;;; Tools

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

;;; Keybindings

(transient-append-suffix 'gptel-menu
  'gptel--suffix-send
  `("a" "Abort active gptel process" gptel-abort
    :if ,(lambda ()
	   (let ((buf (current-buffer)))
	     (cl-find-if
	      (lambda (entry)
		(eq (thread-first (cadr entry)
				  (gptel-fsm-info)
				  (plist-get :buffer))
		    buf))
	      gptel--request-alist)))))

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
	   "shell_command" "grep" "list_directory" "editor_diagnostics")
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

5. You should prefer the dedicated tools over the generic shell_command
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

(defun +gptel-shellcheck (filename)
  "Run shellcheck to analyze shell scripts for errors, bugs, and potential pitfalls.

FILENAME is the path to the shell script file to be analyzed.
Excludes specific checks for source files (SC1091) and unused variables (SC2034).

Returns the shellcheck output as a string."
  (with-temp-buffer
    (process-file "shellcheck" nil t nil
		  (file-local-name (expand-file-name filename)) "--exclude=SC1091,SC2034")
    (buffer-string)))

(gptel-make-tool
 :name "shellcheck"
 :function #'+gptel-shellcheck
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
	(message "gptel-agent session started with coding-agent preset")))))

(defun gptel-agent--setup-context ()
  (dolist (filename (list "AGENTS.md"))
    (let ((filename (expand-file-name filename)))
      (when (file-exists-p filename)
	(gptel-add-file filename)))))

(provide 'gptel-config)
;;; gptel-config.el ends here
