;;; herdr.el --- Herdr integration -*- lexical-binding: t; -*-

;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

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

;; Integrate Herdr (https://herdr.dev/) workspaces and coding agents with
;; Emacs.  Commands in this file start and focus project agents, display
;; their output, send prompts and code, and interrupt their current turns.

;; TUI cleanup, final-output extraction, and blocked-prompt layout
;; assumptions have only been tested with OpenCode.  Other agents may use
;; different terminal layouts.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)

(defgroup herdr nil
  "Integration with Herdr workspaces and coding agents."
  :group 'tools
  :prefix "herdr-")

(defvar herdr-prompt-history nil
  "History list for Herdr prompts.")

(defvar herdr-answer-history nil
  "History list for replies to blocked Herdr agents.")

(defcustom herdr-prompt-input-method nil
  "Input method to use when reading a Herdr prompt.
Nil disables input methods.  A string names an input method, such as
\"chinese-py\", for the prompt minibuffer to inherit.  The original
buffer's input method is not changed."
  :type `(choice
	  (const :tag "No input method" nil)
	  (choice :tag "Specific input method"
		  ,@(mapcar (lambda (name)
			      `(const :tag ,name ,name))
			    (sort (mapcar #'car input-method-alist)
				  #'string-lessp)))
	  (string :tag "Other input method name"))
  :group 'herdr)

(defcustom herdr-focus-after-prompt t
  "Whether to focus the Herdr agent after submitting a prompt.
This changes focus only within Herdr; it does not focus the OS window
containing Herdr.  A focus failure does not cancel the prompt."
  :type 'boolean
  :group 'herdr)

(defcustom herdr-prompt-max-bytes 120000
  "Maximum encoded prompt size passed to Herdr as one argument.
UTF-8 bytes are counted.  The default stays below Linux's usual
128 KiB limit for a single argv entry.  Nil disables the check."
  :type '(choice (const :tag "Unlimited" nil)
		 (natnum :tag "Bytes"))
  :group 'herdr)

(defun herdr--read-with-input-method (prompt history)
  "Read PROMPT with HISTORY using `herdr-prompt-input-method'."
  ;; `current-input-method' is buffer-local, and let-binding it leaks into
  ;; the minibuffer, where `activate-input-method' then sees a non-nil value
  ;; and skips activation, so the input method is never loaded.  Activate it
  ;; explicitly in the minibuffer instead.
  (minibuffer-with-setup-hook
      (lambda ()
        (when herdr-prompt-input-method
          (activate-input-method herdr-prompt-input-method)))
    (read-string prompt nil history)))

(defun herdr--read-prompt ()
  "Read a Herdr prompt."
  (herdr--read-with-input-method "Prompt: " 'herdr-prompt-history))

(defun herdr--read-answer ()
  "Read a manual reply to a blocked Herdr agent."
  (herdr--read-with-input-method "Answer: " 'herdr-answer-history))

(defun herdr--check-prompt-size (prompt)
  "Signal a user error when PROMPT exceeds `herdr-prompt-max-bytes'."
  (let ((bytes (string-bytes prompt)))
    (when (and herdr-prompt-max-bytes
	       (> bytes herdr-prompt-max-bytes))
      (user-error "Herdr prompt is %d bytes; limit is %d bytes"
		  bytes herdr-prompt-max-bytes))))

(defun herdr--call (context &rest args)
  "Run herdr with ARGS and return its output.
Signal an error prefixed with CONTEXT on a non-zero exit status."
  (with-temp-buffer
    (let ((exitcode (apply #'process-file "herdr" nil t nil args)))
      (unless (eq exitcode 0)
	(error "%s: %s" context (string-trim (buffer-string))))
      (buffer-string))))

(defun herdr--call-json (context &rest args)
  "Run herdr with ARGS and parse its JSON output.
Signal an error prefixed with CONTEXT on failure."
  (with-temp-buffer
    (insert (apply #'herdr--call context args))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(defun herdr--project-root ()
  "Return the normalized root of the current project."
  (let* ((project (project-current 'MAYBE-PROMPT))
	 (root (expand-file-name (project-root project)))
	 (root (directory-file-name (file-local-name root))))
    root))

;;;; Agents

(defun herdr-get-project-agent (&optional no-start pane-id)
  "Return a Herdr agent whose working directory is the current project root.
If several agents match, prompt for one.  If none match, offer to start
one in a new workspace unless optional argument NO-START is non-nil.
If optional argument PANE-ID is non-nil, restrict matches to that pane."
  (let* ((root (herdr--project-root))
	 (response (herdr--call-json
		    "Failed to list agents for this project"
		    "agent" "list"))
	 (agents (plist-get (plist-get response :result) :agents))
	 (agents (seq-filter
		  (lambda (agent)
		    (and (equal (plist-get agent :cwd) root)
			 (or (null pane-id)
			     (equal (plist-get agent :pane_id) pane-id))))
		  agents)))
    (cond
     ((null agents)
      (if (and (not no-start)
	       (y-or-n-p
		(format
		 "No herdr agent found for project %s. Start one in a new workspace? "
		 root)))
	  (herdr-start-project-agent root)
	(user-error "No herdr agent found for project: %s" root)))
     ((null (cdr agents))
      (car agents))
     (t
      (let* ((candidates
	      (mapcar (lambda (agent)
			(cons (format "%s (%s, %s)"
				      (plist-get agent :pane_id)
				      (plist-get agent :agent)
				      (plist-get agent :agent_status))
			      agent))
		      agents))
	     (choice (completing-read "Herdr agent: " candidates nil t)))
	(cdr (assoc choice candidates)))))))

(defun herdr-start-project-agent (root &optional focus)
  "Start an agent for project ROOT in a new Herdr workspace.
Prompt for the agent kind.  If optional argument FOCUS is non-nil, focus
the new workspace; otherwise, leave the current focus unchanged.
Start the agent in the workspace's root pane and return it."
  (let* ((kind (completing-read
		"Herdr agent kind: "
		'("pi" "claude" "codex" "gemini" "cursor" "devin" "agy"
		  "cline" "omp" "mastracode" "opencode" "copilot" "kimi"
		  "kiro" "droid" "amp" "grok" "hermes" "kilo" "qodercli"
		  "qwen" "letta" "maki" "muse")
		nil t nil nil "opencode"))
	 (label (file-name-nondirectory (directory-file-name root)))
	 pane-id)
    (setq pane-id
	  (plist-get
	   (plist-get
	    (herdr--call-json
	     "Failed to create herdr workspace"
	     "workspace" "create"
	     "--cwd" root
	     "--label" label
	     (if focus "--focus" "--no-focus"))
	    :result)
	   :root_pane))
    (setq pane-id (plist-get pane-id :pane_id))
    (unless pane-id
      (error "Herdr workspace create returned no root pane"))
    (let ((name (concat "agent-"
			(replace-regexp-in-string
			 "[^a-z0-9_-]" "-" (downcase pane-id)))))
      (herdr--call
       "Failed to start herdr agent"
       "agent" "start"
       (substring name 0 (min 32 (length name)))
       "--kind" kind
       "--pane" pane-id)
      (herdr-get-project-agent 'NO-START pane-id))))

;;;###autoload
(defun herdr-open-new-agent ()
  "Open a new Herdr agent for the current project.
Create and focus a new workspace, then start the agent in its root pane."
  (interactive)
  (herdr-start-project-agent (herdr--project-root) 'FOCUS))

(defun herdr--focus-agent (agent &optional directory)
  "Focus Herdr AGENT, running the command from DIRECTORY."
  (let ((default-directory (or directory default-directory)))
    (herdr--call "Failed to focus herdr agent"
		 "agent" "focus" (plist-get agent :pane_id))))

;;;###autoload
(defun herdr-focus-project-agent ()
  "Focus a Herdr agent for the current project.
Prompt for an agent if several belong to the project, and offer to start
one if none exist.  This changes focus only within Herdr; it does not
focus the OS window containing Herdr."
  (interactive)
  (herdr--focus-agent (herdr-get-project-agent)))

;;;; Agent output

(defcustom herdr-output-final-only t
  "Whether to show only the final assistant prose when available.
This is a best-effort extraction based on Herdr's terminal layout.  It
uses the last `+ Thought' marker as a boundary and removes `┃' blocks
containing user messages, tool calls, and quoted output."
  :type 'boolean
  :group 'herdr)

(defcustom herdr-output-display 'minibuffer
  "How to display Herdr agent output.
`per-agent-buffer' uses a persistent buffer for each agent.
`fixed-buffer' reuses one shared output buffer.
`temp-window' displays the output in a temporary popup window.
`minibuffer' shows a truncated preview in the echo area."
  :type '(choice (const :tag "Buffer per agent" per-agent-buffer)
		 (const :tag "Fixed shared buffer" fixed-buffer)
		 (const :tag "Temporary popup window" temp-window)
		 (const :tag "Minibuffer" minibuffer))
  :group 'herdr)

(defcustom herdr-output-lines 200
  "Default number of lines to read from a Herdr agent."
  :type 'natnum
  :group 'herdr)

(defcustom herdr-output-clean-tui t
  "Whether to remove Herdr terminal UI decorations from output.
Disable this to inspect the raw terminal snapshot."
  :type 'boolean
  :group 'herdr)

(defvar-local herdr--agent-output-agent nil
  "Agent displayed in the current Herdr output buffer.")

(defvar-local herdr--agent-output-lines herdr-output-lines
  "Number of output lines displayed in the current Herdr buffer.")

(defvar herdr-agent-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'herdr-agent-output-refresh)
    map)
  "Keymap for `herdr-agent-output-mode'.")

(define-derived-mode herdr-agent-output-mode special-mode "Herdr-Agent"
  "Major mode for displaying Herdr agent output.")

(defun herdr-agent-output-refresh ()
  "Refresh the Herdr agent output in the current buffer."
  (interactive)
  (unless herdr--agent-output-agent
    (user-error "No Herdr agent associated with this buffer"))
  (herdr--show-agent herdr--agent-output-agent
		     herdr--agent-output-lines
		     default-directory))

(defun herdr--collect-prose-lines (start)
  "Collect lines from START to the end of the buffer, skipping gutters.
Lines whose content starts with the `┃' gutter are dropped.  Return the
remaining lines in order."
  (goto-char start)
  (let (lines)
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
		   (line-beginning-position) (line-end-position))))
	(unless (string-match-p "^[[:space:]]*┃" line)
	  (push line lines)))
      (forward-line 1))
    (nreverse lines)))

(defun herdr--final-agent-output (output)
  "Extract the final assistant prose from Herdr OUTPUT.
Use the last `+ Thought' marker as a best-effort boundary and remove
lines from `┃' gutter blocks.  Return nil when no such section exists."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (let (start)
      (while (re-search-forward "^[[:space:]]*\\+ Thought\\b.*$" nil t)
	(setq start (line-beginning-position 2)))
      (when start
	(let ((text (mapconcat #'identity
			       (herdr--collect-prose-lines start) "\n")))
	  (unless (string-empty-p (string-trim text))
	    text))))))

(defun herdr--clean-agent-output (output)
  "Remove Herdr TUI decorations from OUTPUT."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (while (re-search-forward "\\([^ \t\n]\\)[ \t]\\{24,\\}[^ \t\n].*$" nil t)
      (replace-match (match-string 1) t t))
    (goto-char (point-min))
    (delete-matching-lines
     "^[[:space:]]*\\(?:┃[[:space:]]*\\)?\\(?:Build ·\\|\\+ Thought\\|[⠁-⣿]\\|Thinking\\.\\.\\.\\)")
    (goto-char (point-min))
    (delete-matching-lines "^[[:space:]]*[╹▀]+[[:space:]]*$")
    (goto-char (point-min))
    (delete-matching-lines "^[[:space:]]*⬝")
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*┃[[:space:]]?" nil t)
      (replace-match ""))
    (let ((min-indent most-positive-fixnum))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*[^ \t\n]" nil t)
	(setq min-indent
	      (min min-indent
		   (save-excursion
		     (goto-char (match-beginning 0))
		     (skip-chars-forward " \t")
		     (current-column)))))
      (when (/= min-indent most-positive-fixnum)
	(let ((indent-tabs-mode nil))
	  (indent-rigidly (point-min) (point-max) (- min-indent)))))
    (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward "\n\\(?:[ \t]*\n\\)\\{2,\\}" nil t)
      (replace-match "\n\n"))
    (string-trim (buffer-string))))

(defun herdr--minibuffer-text (text)
  "Return a shortened version of TEXT for the echo area.
Keep the end of long text, where agent status and questions appear."
  (let ((limit 500))
    (if (<= (length text) limit)
	text
      (concat "..." (substring text (- (length text) limit))))))

(defun herdr--read-output (agent lines directory &optional source)
  "Return AGENT output of LINES from the end read from DIRECTORY.
Read from SOURCE, which defaults to the visible screen for working
agents and to recent unwrapped history otherwise.  Fall back to the
visible screen when history is unavailable because the agent is not
idle.  Apply `herdr-output-final-only' and `herdr-output-clean-tui'."
  (let* ((default-directory directory)
	 (source (or source
		     (if (equal (plist-get agent :agent_status) "working")
			 "visible"
		       "recent-unwrapped")))
	 (read (lambda (source)
		 (herdr--call
		  "Failed to read herdr agent output"
		  "agent" "read" (plist-get agent :pane_id)
		  "--source" source "--lines" (number-to-string lines))))
	 (raw
	  (condition-case err
	      (funcall read source)
	    (error
	     (if (and (equal source "recent-unwrapped")
		      (string-match-p "agent_not_idle"
				      (error-message-string err)))
		 (funcall read "visible")
	       (signal err)))))
	 (selected (or (and herdr-output-final-only
			    (herdr--final-agent-output raw))
		       raw)))
    (if herdr-output-clean-tui
	(herdr--clean-agent-output selected)
      selected)))

(defun herdr--show-agent (agent lines directory)
  "Display recent output for AGENT in a read-only buffer.
Show LINES lines and run Herdr commands from DIRECTORY.  Fall back to
the visible screen when Herdr cannot capture alternate-screen history."
  (let ((output (herdr--read-output agent lines directory)))
    (let ((buffer
	   (if (eq herdr-output-display 'fixed-buffer)
	       (get-buffer-create "*Herdr Agent*")
	     (get-buffer-create (format "*herdr-agent:%s*"
					(plist-get agent :pane_id))))))
      (with-current-buffer buffer
	(unless (derived-mode-p 'herdr-agent-output-mode)
	  (herdr-agent-output-mode))
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert output)
	  (goto-char (point-max)))
	(setq-local herdr--agent-output-agent agent)
	(setq-local herdr--agent-output-lines lines)
	(setq-local default-directory
		    (file-name-as-directory
		     (or (plist-get agent :cwd) directory))))
      (pcase herdr-output-display
	('temp-window
	 (display-buffer
	  buffer
	  '((display-buffer-reuse-window display-buffer-pop-up-window)
	    (window-height . 0.4))))
	('minibuffer
	 (message "%s" (herdr--minibuffer-text output)))
	(_
	 (display-buffer buffer)))
      buffer)))

;;;###autoload
(defun herdr-show-project-agent (&optional lines)
  "Show recent output from a Herdr agent for the current project.
With prefix argument LINES, show that many lines; otherwise, use
`herdr-output-lines'.
Working agents may show only the visible screen when history is
unavailable.  The output buffer is read-only, and `g' refreshes it."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (herdr--show-agent (herdr-get-project-agent)
		     (or lines herdr-output-lines)
		     default-directory))

;;;###autoload
(defun herdr-ask (agent prompt &optional lines)
  "Send PROMPT to Herdr AGENT and asynchronously wait for a result.
Display recent output when the agent finishes or requests input.
Optional argument LINES is the number of output lines to display and
defaults to `herdr-output-lines'.  Return the process waiting for Herdr."
  (interactive (list (herdr-get-project-agent)
		     (herdr--read-prompt)
		     (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (herdr--check-prompt-size prompt)
  (let* ((directory default-directory)
	 (lines (or lines herdr-output-lines))
	 (buffer (generate-new-buffer " *herdr-ask*"))
	 (process (start-file-process
		   "herdr-ask" buffer "herdr"
		   "agent" "prompt" (plist-get agent :pane_id) prompt
		   "--wait")))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (process _event)
       (unless (process-live-p process)
	 (let* ((buffer (process-buffer process))
		(status (process-exit-status process))
		(output (if (buffer-live-p buffer)
			    (with-current-buffer buffer
			      (buffer-string))
			  "")))
	   (unwind-protect
	       (if (zerop status)
		   (condition-case err
		       (herdr--show-result agent lines directory)
		     (error
		      (message "Failed to show Herdr agent output: %s"
			       (error-message-string err))))
		 (message "Herdr agent prompt failed (%d): %s"
			  status output))
	     (when (buffer-live-p buffer)
	       (kill-buffer buffer)))))))
    (when herdr-focus-after-prompt
      (condition-case err
	  (herdr--focus-agent agent directory)
	(error
	 (message "Failed to focus Herdr agent: %s"
		  (error-message-string err)))))
    (message "Waiting for Herdr agent %s..." (plist-get agent :pane_id))
    process))

;;;###autoload
(defun herdr-interrupt-project-agent ()
  "Interrupt the current turn of a Herdr agent for the current project.
Prompt for an agent if several belong to the project, and ask for
confirmation before sending Ctrl-C."
  (interactive)
  (let ((agent (herdr-get-project-agent)))
    (when (y-or-n-p (format "Send Ctrl-C to Herdr agent %s? "
			    (plist-get agent :pane_id)))
      (herdr--call "Failed to interrupt herdr agent"
		   "agent" "send-keys" (plist-get agent :pane_id) "ctrl+c")
      (message "Sent Ctrl-C to Herdr agent %s"
	       (plist-get agent :pane_id)))))

(defun herdr--agent-info (pane-id)
  "Return Herdr's current state plist for PANE-ID."
  (plist-get
   (plist-get
    (herdr--call-json "Failed to get herdr agent" "agent" "get" pane-id)
    :result)
   :agent))

;;;; Question prompts

(defcustom herdr-question-auto-display t
  "Whether to poll Herdr for blocked agents and show their questions.
When non-nil, `herdr-question-auto-mode' is enabled when this file is
loaded interactively."
  :type 'boolean
  :group 'herdr)

(defcustom herdr-question-poll-interval 2
  "Seconds between checks for agents waiting at a question.
Only blocked agents are read, and each blocked state is shown once."
  :type 'natnum
  :group 'herdr)

(defvar herdr-question-fetch-functions nil
  "Hook run to find an agent's pending question.
Each function is called with a Herdr agent plist and returns a form
plist \(:session-id SID :tool-id TID :questions (Q...)) where each Q
is a display plist with :header, :text, and :options, or nil when the
agent has no pending question.  The first non-nil result is used.
Register agent-specific integrations with `add-hook', for example
`herdr-opencode'.")

(defvar herdr-question-status-functions nil
  "Hook run to check a question's status.
Each function is called with (SESSION-ID TOOL-ID) and returns a status
string such as \"running\", \"completed\", or \"error\", or nil when it
does not recognize the question.  The first non-nil result is used.
Register agent-specific integrations with `add-hook', for example
`herdr-opencode'.")

(defvar-local herdr--question-agent nil
  "Agent whose question is displayed in the current buffer.")

(defvar-local herdr--question-form nil
  "Full question form shown in the current buffer.
A plist (:session-id SID :tool-id TID :questions (Q...)) where each Q
is (:header :text :options).")

(defvar-local herdr--question-index nil
  "Index of the question currently shown, or nil on the review page.")

(defvar-local herdr--question-answers nil
  "Vector of answers indexed by question position.
Element i is the answer for question i: nil when unanswered, a label
string for a single-select question, or a list of labels for a
multi-select question.")

(defvar-local herdr--question-selected nil
  "Labels selected so far for the current multi-select question.")

(defvar-local herdr--question-data nil
  "Question plist currently displayed, or the review plist.")

(defvar-local herdr--question-answered nil
  "Non-nil while a submitted answer is being checked.")

(defvar-local herdr--question-retries 0
  "Number of completion checks after the final submit.")

(defvar herdr--question-next-timeout 10
  "Seconds to wait for a submitted answer to be recorded.")

(defvar-local herdr--question-pending nil
  "Time until which the popup waits for a submitted answer.")

(defvar herdr--question-window nil
  "Window currently displaying a Herdr question buffer.")

(defvar herdr--question-timer nil
  "Timer used by `herdr-question-auto-mode'.")

(defvar herdr--question-seen nil
  "Alist mapping pane IDs to the blocked state already displayed.")

(defvar herdr-question-mode-map
  (let ((map (make-sparse-keymap)))
    (dotimes (index 9)
      (define-key map (number-to-string (1+ index)) #'herdr-question-select))
    (define-key map (kbd "RET") #'herdr-question-submit)
    (define-key map (kbd "i") #'herdr-question-answer-text)
    (define-key map (kbd "left") #'herdr-question-prev)
    (define-key map (kbd "l") #'herdr-question-prev)
    (define-key map (kbd "right") #'herdr-question-next)
    (define-key map (kbd "r") #'herdr-question-next)
    map)
  "Keymap for `herdr-question-mode'.")

(define-derived-mode herdr-question-mode special-mode "Herdr-Question"
  "Major mode for answering a Herdr agent question.
Use \\[revert-buffer] to refresh the question and \\[quit-window] to
bury the popup without answering."
  (setq-local revert-buffer-function #'herdr-question-refresh))

(defun herdr--insert-question (question)
  "Insert parsed QUESTION into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (plist-get question :review)
	(progn
	  (insert (propertize "All questions answered\n\n" 'face 'bold))
	  (dolist (item (plist-get question :items))
	    (insert (format "  %s: %s\n" (car item) (cdr item))))
	  (insert "\n")
	  (insert (propertize "RET submit · ← back · g refresh · q quit"
			      'face 'shadow)))
      (let ((options (plist-get question :options))
	    (multiple (plist-get question :multiple)))
	(insert (propertize "Question\n" 'face 'bold))
	(insert (plist-get question :text)
		(if multiple " (select all that apply)" "")
		"\n\n")
	(cl-loop for option in options
		 for index from 1
		 for label = (plist-get option :label)
		 for picked = (and (not multiple)
				   (equal label
					  (aref herdr--question-answers
						herdr--question-index)))
		 do (insert (propertize
			     (format "%d. %s%s%s\n" index
				     (if multiple
					 (format "[%s] "
						 (if (member label herdr--question-selected)
						     "x" " "))
				       "")
				     label
				     (if picked " ✓" ""))
			     'face 'bold))
		    (when-let* ((description
				 (plist-get option :description)))
		      (insert (concat "   "
				      (replace-regexp-in-string
				       "\n" "\n   " description)
				      "\n")))
		    (insert "\n"))
	(unless multiple
	  ;; Make the free-text input an explicit, visible choice so it is
	  ;; obvious the question accepts a typed answer, not just a number.
	  (insert (propertize
		   (format "%d. Type your own answer\n" (1+ (length options)))
		   'face 'bold)
		  "\n"))
	(insert (propertize
		 (if multiple
		     "1-9 toggle · ←/→ nav · RET done · g refresh · q quit"
		   "1-9 select · i input · ←/→ nav · RET enter · g refresh · q quit")
		 'face 'shadow))))
    (goto-char (point-min))))

(defconst herdr--question-display-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . 0.35))
  "How to display a Herdr question popup.")

(defun herdr--display-question (buffer)
  "Display question BUFFER in a single popup window below the selected one."
  (let ((window (and (window-live-p herdr--question-window)
		     herdr--question-window)))
    (cond
     ((null window)
      (setq window (display-buffer buffer herdr--question-display-action))
      (setq herdr--question-window window))
     ((eq (window-buffer window) buffer)
      nil)
     ((let ((current (window-buffer window)))
	(and (buffer-live-p current)
	     (with-current-buffer current
	       (derived-mode-p 'herdr-question-mode))))
      (set-window-buffer window buffer))
     (t
      (setq window (display-buffer buffer herdr--question-display-action))
      (setq herdr--question-window window)))
    (when (window-live-p window)
      (select-window window))))

(defun herdr--close-question-buffer (buffer)
  "Kill question BUFFER, its popup window, and forget that window."
  (let ((window (and (window-live-p herdr--question-window)
		     herdr--question-window)))
    (when (and window
	       (eq (window-buffer window) buffer))
      (setq herdr--question-window nil)
      ;; `delete-window' signals when this is the only window; in that
      ;; case keeping the frame is preferable to failing to close.
      (condition-case nil
	  (delete-window window)
	(error nil))))
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun herdr--validate-question-form (form)
  "Signal an error if FORM is not a well-formed question form.
A valid form has :session-id, :tool-id, and a non-empty :questions
list.  This guards against a provider returning malformed data."
  (unless (and (plist-get form :session-id)
               (plist-get form :tool-id)
               (consp (plist-get form :questions)))
    (error "Invalid question form from provider: %S" form)))

(defun herdr--show-question (agent)
  "Display AGENT's pending question via `herdr-question-fetch-functions'.
Return the buffer, or nil when AGENT has no pending question."
  (when-let* ((found (run-hook-with-args-until-success
                      'herdr-question-fetch-functions agent)))
    (herdr--validate-question-form found)
    (let* ((questions (plist-get found :questions))
           (buffer (get-buffer-create
                    (format "*Herdr Question: %s*"
                            (plist-get agent :pane_id)))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'herdr-question-mode)
          (herdr-question-mode))
        (setq-local default-directory
                    (file-name-as-directory
                     (or (plist-get agent :cwd) default-directory)))
        (setq-local herdr--question-agent agent)
        (setq-local herdr--question-form
                    (list :session-id (plist-get found :session-id)
                          :tool-id (plist-get found :tool-id)
                          :questions questions))
        (setq-local herdr--question-index 0)
        (setq-local herdr--question-answers (make-vector (length questions) nil))
        (setq-local herdr--question-selected nil)
        (setq-local herdr--question-answered nil)
        (setq-local herdr--question-retries 0)
        (setq-local herdr--question-pending nil)
        (setq-local herdr--question-data (car questions))
        (herdr--insert-question (car questions)))
      (herdr--display-question buffer)
      buffer)))


(defun herdr--show-result (agent lines directory)
  "Show AGENT's question prompt, or its output when it is not asking.
Use LINES and DIRECTORY when reading output."
  (let* ((info (condition-case nil
		   (herdr--agent-info (plist-get agent :pane_id))
		 (error nil)))
	 (question
	  (and info
	       (equal (plist-get info :agent_status) "blocked")
	       (herdr--show-question agent))))
    (when question
      (setq herdr--question-seen
	    (cons (cons (plist-get agent :pane_id)
			(plist-get info :state_change_seq))
		  (assq-delete-all (plist-get agent :pane_id)
				   herdr--question-seen))))
    (or question
	(herdr--show-agent agent lines directory))))

(defun herdr--question-poll ()
  "Keep question buffers current and display newly blocked agents."
  (with-demoted-errors "Herdr question poll error: %S"
    (let* ((response (herdr--call-json
		      "Failed to list herdr agents" "agent" "list"))
	   (agents (plist-get (plist-get response :result) :agents))
	   (blocked (seq-filter
		     (lambda (agent)
		       (equal (plist-get agent :agent_status) "blocked"))
		     agents))
	   (blocked-ids (mapcar (lambda (agent)
				  (plist-get agent :pane_id))
				blocked)))
      ;; Keep every displayed question in sync with its pane, even when
      ;; Herdr reports the agent as working between questions.  This also
      ;; closes buffers for dismissed forms.
      (dolist (buffer (buffer-list))
	(when (string-match "\\`\\*Herdr Question: \\(.+\\)\\*\\'"
			    (buffer-name buffer))
	  (with-current-buffer buffer
	    (when (and herdr--question-agent
		       (derived-mode-p 'herdr-question-mode))
	      (condition-case nil
		  (herdr-question-refresh)
		(error nil))))))
      ;; Forget agents that are no longer blocked, so that their next
      ;; blocked state is shown again.
      (setq herdr--question-seen
	    (seq-filter
	     (lambda (entry) (member (car entry) blocked-ids))
	     herdr--question-seen))
      (dolist (agent blocked)
	(let* ((pane-id (plist-get agent :pane_id))
	       (sequence (plist-get agent :state_change_seq))
	       (known (assoc pane-id herdr--question-seen))
	       (buffer (get-buffer
			(format "*Herdr Question: %s*" pane-id))))
	  (unless (and known
		       (equal (cdr known) sequence)
		       (buffer-live-p buffer))
	    (herdr--show-question agent)
	    (setq herdr--question-seen
		  (cons (cons pane-id sequence)
			(assq-delete-all pane-id herdr--question-seen)))))))))

;;;###autoload
(define-minor-mode herdr-question-auto-mode
  "Automatically display Herdr question prompts in Emacs.
While enabled, poll Herdr every `herdr-question-poll-interval' seconds
and pop up a buffer for each newly blocked agent that shows a question."
  :global t
  :group 'herdr
  (if herdr-question-auto-mode
      (unless (timerp herdr--question-timer)
	(if (and (numberp herdr-question-poll-interval)
		 (> herdr-question-poll-interval 0))
	    (setq herdr--question-timer
		  (run-with-timer 0 herdr-question-poll-interval
				  #'herdr--question-poll))
	  (setq herdr-question-auto-mode nil)))
    (when (timerp herdr--question-timer)
      (cancel-timer herdr--question-timer))
    (setq herdr--question-timer nil)))

(defun herdr--question-tool-status (session-id tool-id)
  "Return the question's status string via `herdr-question-status-functions'.
SESSION-ID and TOOL-ID identify the question tool call.  Signal an
error if a provider returns a non-string, non-nil value."
  (let ((status (run-hook-with-args-until-success
                 'herdr-question-status-functions session-id tool-id)))
    (when (and status (not (stringp status)))
      (error "Invalid question status from provider: %S" status))
    status))

(defun herdr-question-refresh (&optional silent)
  "Re-check the current question via `herdr-question-status-functions'.
Close the buffer once its question tool call is no longer running.
Return non-nil while the form is still running.  With SILENT, do not
report the closure."
  (interactive)
  (unless herdr--question-agent
    (user-error "No Herdr question associated with this buffer"))
  (let* ((buffer (current-buffer))
         (form herdr--question-form)
         (status (and form
                      (herdr--question-tool-status
                       (plist-get form :session-id)
                       (plist-get form :tool-id)))))
    (if (or (null status) (equal status "running"))
	t
      (progn
	(setq herdr--question-seen
	      (assq-delete-all (plist-get herdr--question-agent :pane_id)
			       herdr--question-seen))
	(herdr--close-question-buffer buffer)
	(unless silent
	  (message "Herdr agent is no longer asking a question"))
	nil))))

(defun herdr--hide-question-window ()
  "Delete the question popup window while keeping its buffer."
  (let ((window (and (window-live-p herdr--question-window)
		     herdr--question-window)))
    (when (and window
	       (eq (window-buffer window) (current-buffer)))
      (setq herdr--question-window nil)
      (condition-case nil
	  (delete-window window)
	(error nil)))))

(defun herdr--send-keys-async (pane-id keys &optional on-error)
  "Send KEYS to PANE-ID in the background, then call ON-ERROR on failure."
  (make-process
   :name "herdr-send-keys"
   :buffer nil
   :command (append (list "herdr" "agent" "send-keys" pane-id) keys)
   :connection-type 'pipe
   :noquery t
   :sentinel
   (lambda (process _event)
     (when-let* (((memq (process-status process) '(exit signal)))
                 (status (process-exit-status process))
                 ((and on-error
                       (or (not (integerp status)) (/= status 0)))))
       (funcall on-error)))))

(defun herdr--question-answer-failed (buffer)
  "Show question BUFFER again after its answer could not be sent."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq herdr--question-answered nil
	    herdr--question-retries 0)
      (herdr--display-question buffer)
      (message "Failed to send Herdr answer; please answer again"))))

(defun herdr--question-assert ()
  "Signal a user error unless a Herdr question is active and unanswered."
  (unless herdr--question-agent
    (user-error "No Herdr question in this buffer"))
  (when herdr--question-answered
    (user-error "Answer already sent; waiting for the agent")))

(defmacro herdr--question-with-rollback (&rest body)
  "Run BODY, clearing the answered flag and re-signaling on error."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error
      (setq herdr--question-answered nil)
      (signal err))))

(defun herdr-question-select (&optional number)
  "Select or toggle option NUMBER in the current question buffer.
For a multi-select question, NUMBER toggles the option on or off.  For
a single-select question, NUMBER picks the option and submits or
advances.  Interactively, use the pressed digit, or prompt when called
otherwise."
  (interactive
   (list (if (and (integerp last-command-event)
		  (<= ?1 last-command-event ?9))
	     (- last-command-event ?0)
	   (read-number "Option number: "))))
  (herdr--question-assert)
  (when (plist-get herdr--question-data :review)
    (user-error "All questions answered; press RET to submit"))
  (let* ((options (plist-get herdr--question-data :options))
	 (count (length options))
	 (multiple (plist-get herdr--question-data :multiple))
	 ;; Single-select questions show an extra "Type your own answer"
	 ;; entry at N+1; multi-select questions only have the options.
	 (max-option (if multiple count (1+ count))))
    (unless (and (>= number 1) (<= number max-option))
      (user-error "No option %d; choose 1-%d" number max-option))
    (cond
     (multiple
      (herdr--question-toggle number options))
     ((= number (1+ count))
      (herdr-question-answer-text))
     (t
      (herdr--question-pick number options)))))

(defun herdr--question-toggle (number options)
  "Toggle option NUMBER (1-based) from OPTIONS in this multi-select question.
Send the digit to the TUI immediately so both stay in sync, and record
the selection live so navigation can restore it."
  (let ((label (plist-get (nth (1- number) options) :label))
	(pane-id (plist-get herdr--question-agent :pane_id)))
    (herdr--call "Failed to toggle question option"
		 "agent" "send-keys" pane-id (number-to-string number))
    (setq herdr--question-selected
	  (if (member label herdr--question-selected)
	      (remove label herdr--question-selected)
	    (append herdr--question-selected (list label))))
    (aset herdr--question-answers herdr--question-index
	  herdr--question-selected)
    (herdr--insert-question herdr--question-data)))

(defun herdr--question-restore-selection ()
  "Restore `herdr--question-selected' from the recorded answer."
  (setq herdr--question-selected
	(let ((answer (aref herdr--question-answers herdr--question-index)))
	  (if (listp answer) answer nil))))

(defun herdr--question-goto (index)
  "Display question INDEX and restore its recorded selection."
  (let ((questions (plist-get herdr--question-form :questions)))
    (setq herdr--question-index index
	  herdr--question-data (nth index questions))
    (herdr--question-restore-selection)
    (herdr--insert-question herdr--question-data)))

(defun herdr--question-pick (number options)
  "Pick option NUMBER (1-based) from OPTIONS and submit or advance."
  (let* ((label (plist-get (nth (1- number) options) :label))
	 (pane-id (plist-get herdr--question-agent :pane_id))
	 (buffer (current-buffer))
	 (questions (plist-get herdr--question-form :questions))
	 (nquestions (length questions))
	 (index herdr--question-index)
	 (keys (list (number-to-string number))))
    (aset herdr--question-answers index label)
    (setq herdr--question-answered t
	  herdr--question-retries 0)
    (cond
     ((= nquestions 1)
      ;; A single question submits the form as soon as it is chosen.
      (herdr--question-with-rollback
	(herdr--send-keys-async
	 pane-id keys
	 (lambda () (herdr--question-answer-failed buffer)))
	(herdr--hide-question-window)
	(redisplay t))
      (herdr--question-watch-completion buffer))
     ((< index (1- nquestions))
      ;; More questions remain: advance to the next one.
      (herdr--question-with-rollback
	(herdr--call "Failed to send question answer"
		     "agent" "send-keys" pane-id (car keys)))
      (setq herdr--question-answered nil)
      (herdr--question-goto (1+ index)))
     (t
      ;; Last question answered: show the review page.
      (herdr--question-with-rollback
	(herdr--call "Failed to send question answer"
		     "agent" "send-keys" pane-id (car keys)))
      (herdr--question-show-review)))))

(defun herdr--question-show-review ()
  "Show the review page built from the answers collected so far."
  (let ((questions (plist-get herdr--question-form :questions))
	(items nil))
    (cl-loop for question in questions
	     for index from 0
	     for answer = (aref herdr--question-answers index)
	     do (push (cons (plist-get question :header)
			    (cond
			     ((listp answer) (string-join answer ", "))
			     (answer answer)
			     (t "(not answered)")))
		      items))
    (setq herdr--question-index nil
	  herdr--question-answered nil
	  herdr--question-data (list :review t :items (nreverse items)))
    (herdr--insert-question herdr--question-data)))

(defvar-local herdr--question-after-timer nil
  "Timer checking whether a submitted answer was recorded, or nil.")

(defun herdr--question-watch-completion (buffer)
  "Watch BUFFER's question tool call until it is no longer running."
  (let ((session-id (plist-get herdr--question-form :session-id))
	(tool-id (plist-get herdr--question-form :tool-id)))
    (when (timerp herdr--question-after-timer)
      (cancel-timer herdr--question-after-timer))
    (setq herdr--question-pending
	  (+ (float-time) herdr--question-next-timeout))
    (setq herdr--question-after-timer
	  (run-at-time
	   0.5 0.5
	   (lambda ()
	     (when (buffer-live-p buffer)
	       (herdr--question-check-completion buffer session-id tool-id)))))))

(defun herdr--question-stop-watching ()
  "Cancel the completion-watch timer in the current buffer, if any."
  (when (timerp herdr--question-after-timer)
    (cancel-timer herdr--question-after-timer))
  (setq herdr--question-after-timer nil))

(defun herdr--question-check-completion (buffer session-id tool-id)
  "Check whether BUFFER's question TOOL-ID has been recorded.
SESSION-ID and TOOL-ID are captured at submit time so a reused buffer
cannot be closed by a stale timer."
  (cond
   ((not (buffer-live-p buffer))
    ;; Buffer is gone: stop watching.
    (herdr--question-stop-watching))
   ((not (with-current-buffer buffer
	   (equal (plist-get herdr--question-form :tool-id) tool-id)))
    ;; Buffer was reused for a new form: stop watching.
    (herdr--question-stop-watching))
   (t
    (with-current-buffer buffer
      (let ((status (herdr--question-tool-status session-id tool-id)))
	(cond
	 ((and status (not (equal status "running")))
	  ;; Recorded (completed) or dismissed (error); done.
	  (herdr--question-stop-watching)
	  (herdr--close-question-buffer buffer))
	 ((< (float-time) herdr--question-pending)
	  ;; Still running; keep waiting.
	  nil)
	 (t
	  ;; The answer never took effect; show it again.
	  (herdr--question-stop-watching)
	  (herdr--question-answer-failed buffer))))))))

(defun herdr-question-submit ()
  "Submit the current question.
On the review page, send Enter to submit the form.  On a multi-select
question, finalize the selection and advance."
  (interactive)
  (herdr--question-assert)
  (if (and (not (plist-get herdr--question-data :review))
	   (plist-get herdr--question-data :multiple))
      (herdr--question-finalize-multiple)
    (unless (plist-get herdr--question-data :review)
      (user-error "Not on the review page; select an option first"))
    (let ((pane-id (plist-get herdr--question-agent :pane_id))
	  (buffer (current-buffer)))
      (setq herdr--question-answered t
	    herdr--question-retries 0)
      (herdr--question-with-rollback
	(herdr--send-keys-async
	 pane-id '("enter")
	 (lambda () (herdr--question-answer-failed buffer)))
	(herdr--hide-question-window)
	(redisplay t))
      (herdr--question-watch-completion buffer))))

(defun herdr-question-answer-text ()
  "Answer the current single-select question with free text.
Prompt for the text in the minibuffer and send it to the agent."
  (interactive)
  (herdr--question-assert)
  (when (plist-get herdr--question-data :review)
    (user-error "All questions answered; press RET to submit"))
  (when (plist-get herdr--question-data :multiple)
    (user-error "Free text is not available for multi-select questions"))
  (let* ((header (plist-get herdr--question-data :header))
	 (text (herdr--read-with-input-method (format "%s: " header) nil))
	 (options (plist-get herdr--question-data :options))
	 (digit (number-to-string (1+ (length options))))
	 (pane-id (plist-get herdr--question-agent :pane_id))
	 (buffer (current-buffer))
	 (questions (plist-get herdr--question-form :questions))
	 (nquestions (length questions))
	 (index herdr--question-index))
    (when (string-empty-p text)
      (user-error "Empty answer"))
    (aset herdr--question-answers index text)
    (setq herdr--question-answered t
	  herdr--question-retries 0)
    (herdr--question-with-rollback
      ;; Open the "Type your own answer" editor, type the text, submit.
      (herdr--call "Failed to open custom answer"
		   "agent" "send-keys" pane-id digit)
      (herdr--call "Failed to send custom answer"
		   "pane" "send-text" pane-id text)
      (herdr--call "Failed to submit custom answer"
		   "agent" "send-keys" pane-id "enter"))
    (cond
     ((= nquestions 1)
      (herdr--hide-question-window)
      (redisplay t)
      (herdr--question-watch-completion buffer))
     ((< index (1- nquestions))
      (setq herdr--question-answered nil)
      (herdr--question-goto (1+ index)))
     (t
      (herdr--question-show-review)))))

(defun herdr--question-finalize-multiple ()
  "Advance from the current multi-select question.
The selection was already sent and recorded while toggling, so only
send the navigation key and move to the next question or review."
  (let* ((pane-id (plist-get herdr--question-agent :pane_id))
	 (questions (plist-get herdr--question-form :questions))
	 (nquestions (length questions))
	 (index herdr--question-index))
    (unless herdr--question-selected
      (user-error "Select at least one option"))
    (setq herdr--question-answered t
	  herdr--question-retries 0)
    (herdr--question-with-rollback
      (herdr--call "Failed to advance question"
		   "agent" "send-keys" pane-id "right"))
    (if (< index (1- nquestions))
	(progn
	  (setq herdr--question-answered nil)
	  (herdr--question-goto (1+ index)))
      (herdr--question-show-review))))

(defun herdr-question-next ()
  "Move to the next question, or the review page from the last question.
The navigation key is also sent to the agent so both stay in sync."
  (interactive)
  (herdr--question-assert)
  (let* ((questions (plist-get herdr--question-form :questions))
	 (nquestions (length questions))
	 (index herdr--question-index)
	 (pane-id (plist-get herdr--question-agent :pane_id)))
    (when index
      (herdr--call "Failed to navigate question"
		   "agent" "send-keys" pane-id "right")
      (if (< index (1- nquestions))
	  (herdr--question-goto (1+ index))
	(herdr--question-show-review)))))

(defun herdr-question-prev ()
  "Move to the previous question, or from review to the last question.
The navigation key is also sent to the agent so both stay in sync."
  (interactive)
  (herdr--question-assert)
  (let* ((questions (plist-get herdr--question-form :questions))
	 (nquestions (length questions))
	 (index herdr--question-index)
	 (pane-id (plist-get herdr--question-agent :pane_id)))
    (cond
     ((null index)
      ;; On the review page: go back to the last question.
      (herdr--call "Failed to navigate question"
		   "agent" "send-keys" pane-id "left")
      (herdr--question-goto (1- nquestions)))
     ((> index 0)
      (herdr--call "Failed to navigate question"
		   "agent" "send-keys" pane-id "left")
      (herdr--question-goto (1- index))))))

;;;###autoload
(defun herdr-answer-project-agent (&optional use-keys)
  "Manually answer a blocked Herdr agent for the current project.
Only act when the agent is waiting at a question or approval prompt.
Without USE-KEYS, read text and send it followed by Enter.  With a
prefix argument, send a space-separated sequence of logical key names
instead."
  (interactive "P")
  (let* ((agent (herdr-get-project-agent 'NO-START))
	 (pane-id (plist-get agent :pane_id))
	 (info (herdr--agent-info pane-id)))
    (unless (equal (plist-get info :agent_status) "blocked")
      (user-error "Herdr agent %s is not waiting for input" pane-id))
    (let ((herdr-output-final-only nil)
	  (herdr-output-display 'temp-window))
      (herdr--show-agent agent (or herdr-output-lines 200)
			 default-directory))
    (if use-keys
	(let ((keys (split-string (read-string "Keys: ") nil t)))
	  (unless keys
	    (user-error "No keys specified"))
	  (apply #'herdr--call "Failed to send keys to herdr agent"
		 "agent" "send-keys" pane-id keys))
      (let ((answer (herdr--read-answer)))
	(herdr--call "Failed to send text to herdr pane"
		     "pane" "send-text" pane-id answer)
	(herdr--call "Failed to submit herdr answer"
		     "agent" "send-keys" pane-id "enter")))
    (message "Sent response to Herdr agent %s" pane-id)))

;;;; Sending text

(defcustom herdr-region-max-length 1000
  "Maximum number of characters in a region to send in full.
Longer regions in file buffers are replaced with the file's line range
and a numbered preview so the agent can read the complete text itself.
Other buffers get a shortened excerpt.  Nil means that regions are
always sent in full."
  :type '(choice (const :tag "Unlimited" nil)
		 (natnum :tag "Characters"))
  :group 'herdr)

(defcustom herdr-region-preview-lines 5
  "Number of lines previewed at each end of a long file region.
The agent is told which file range to read for the complete text."
  :type 'natnum
  :group 'herdr)

(defcustom herdr-region-preview-width 120
  "Maximum display width of a previewed line.
Longer lines are truncated with an ellipsis.  Nil disables truncation."
  :type '(choice (const :tag "Unlimited" nil)
		 (natnum :tag "Columns"))
  :group 'herdr)

(defun herdr--preview-line ()
  "Return the current line as a numbered preview line."
  (let ((text (string-trim-right
	       (buffer-substring-no-properties
		(line-beginning-position) (line-end-position)))))
    (when (and herdr-region-preview-width
	       (> herdr-region-preview-width 0)
	       (> (string-width text) herdr-region-preview-width))
      (setq text (concat
		  (truncate-string-to-width
		   text herdr-region-preview-width nil nil "…"))))
    (format "%5d  %s" (line-number-at-pos (point) t) text)))

(defun herdr--preview-lines (first-line count)
  "Return COUNT numbered lines starting at absolute FIRST-LINE."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- first-line))
      (let (lines)
	(dotimes (_ count)
	  (unless (eobp)
	    (push (herdr--preview-line) lines)
	    (forward-line 1)))
	(string-join (nreverse lines) "\n")))))

(defun herdr--line-count-description (lines)
  "Return LINES as a singular or plural description."
  (format "%d %s" lines (if (= lines 1) "line" "lines")))

(defun herdr--file-region-summary (beg end length lines)
  "Return a summary of a long file region from BEG to END.
LENGTH is the character count and LINES the number of lines.  Point the
agent at the file range and number the previewed lines."
  (let* ((start-line (line-number-at-pos beg t))
	 (end-line (save-excursion
		     (save-restriction
		       (widen)
		       (goto-char end)
		       (when (and (bolp) (not (bobp)))
			 (backward-char))
		       (line-number-at-pos (point) t))))
	 (file (herdr--project-file-name))
	 (preview (max 1 (min herdr-region-preview-lines lines)))
	 (reference
	  (format
	   "[Full region omitted: %d characters, %s. Read %s lines %d-%d for the complete text.]"
	   length (herdr--line-count-description lines)
	   file start-line end-line)))
    (if (<= lines (* 2 preview))
	(format "%s\n\n%s" reference
		(herdr--preview-lines start-line lines))
      (format
       "%s\n\nFirst %d lines:\n%s\n\n... %d lines omitted ...\n\nLast %d lines:\n%s"
       reference preview
       (herdr--preview-lines start-line preview)
       (- lines (* 2 preview))
       preview
       (herdr--preview-lines (1+ (- end-line preview)) preview)))))

(defun herdr--buffer-region-summary (beg end length lines)
  "Return a shortened excerpt of a long non-file region.
BEG and END delimit the region, LENGTH is its character count, and
LINES is its line count."
  (if-let* ((excerpt-length (and (<= lines 4) (min 500 (/ length 4)))))
      (if (zerop excerpt-length)
	  (format "[Full region omitted: %d characters, %d lines.]"
		  length lines)
	(format
	 "[Full region omitted: %d characters, %d lines.]\n\nFirst %d characters:\n%s\n\n... %d characters omitted ...\n\nLast %d characters:\n%s"
	 length lines
	 excerpt-length
	 (buffer-substring-no-properties beg (+ beg excerpt-length))
	 (- length (* 2 excerpt-length))
	 excerpt-length
	 (buffer-substring-no-properties (- end excerpt-length) end)))
    (let (head-end tail-beg)
      (save-excursion
	(goto-char beg)
	(forward-line 2)
	(setq head-end (min (point) end))
	(goto-char end)
	(if (bolp)
	    (forward-line -2)
	  (forward-line -1))
	(setq tail-beg (max (point) beg)))
      (format
       "[Full region omitted: %d characters, %d lines.]\n\nFirst two lines:\n%s\n... %d lines omitted ...\nLast two lines:\n%s"
       length lines
       (buffer-substring-no-properties beg head-end)
       (- lines 4)
       (buffer-substring-no-properties tail-beg end)))))

(defun herdr--region-text (beg end)
  "Return text from BEG to END, or a preview when it is too long.
For file buffers the preview names the file line range so the agent can
read the complete text; other buffers get a shortened excerpt."
  (let* ((length (- end beg))
	 (lines (count-lines beg end)))
    (if (or (null herdr-region-max-length)
	    (<= length herdr-region-max-length))
	(buffer-substring-no-properties beg end)
      (if (buffer-file-name)
	  (herdr--file-region-summary beg end length lines)
	(herdr--buffer-region-summary beg end length lines)))))

(defun herdr--project-file-name ()
  "Return the current file name relative to its project root.
Fall back to the file's local absolute name when there is no project
or the file lies outside it."
  (when-let* ((file (buffer-file-name)))
    (let* ((project (project-current))
	   (root (and project (project-root project)))
	   (relative (and root (file-relative-name file root))))
      (file-local-name
       (or (and relative (not (string-prefix-p ".." relative)) relative)
	   file)))))

(defun herdr--buffer-location ()
  "Return a location label for the current buffer.
Use the project-relative file name when the buffer visits a file.
Otherwise identify it explicitly as an Emacs buffer, with the buffer
name quoted so its boundaries are clear."
  (or (herdr--project-file-name)
      (format "Emacs buffer \"%s\"" (buffer-name))))

(defun herdr--agent-remote ()
  "Return the remote identifier of the agent's host, or nil when local.
The agent runs where the buffer's file or default directory lives, so a
TRAMP file or directory means a remote agent."
  (file-remote-p (or (buffer-file-name) default-directory)))

(defun herdr--dump-buffer-remote ()
  "Write the current buffer to a temp file on the remote host.
Return the file's remote-local path, i.e. what the remote agent types."
  (let* ((remote (herdr--agent-remote))
         (file (make-temp-file (concat remote "/tmp/herdr-buffer-")
                               nil ".txt")))
    (write-region (point-min) (point-max) file nil 'silent)
    (file-local-name file)))

(defun herdr--buffer-read-hint ()
  "Return a hint telling the agent how to read the current buffer, or nil.
File-backed buffers need no hint.  A local non-file buffer gets an
`emacsclient' command; a remote (TRAMP) non-file buffer gets a temp file
dumped to the remote host."
  (unless (buffer-file-name)
    (if (herdr--agent-remote)
        (format "\n\nThe full buffer contents are at:\n  %s"
                (herdr--dump-buffer-remote))
      (format
       "\n\nRead the full buffer with:\n  emacsclient --eval '(with-current-buffer %S (buffer-string))'"
       (buffer-name)))))

(defun herdr--ask-region-prompt (beg end instructions full &optional kind)
  "Build the prompt text for the region between BEG and END.
INSTRUCTIONS, when non-nil, is prepended.  When FULL is non-nil, send
the full region text; otherwise send a summary of long regions.  KIND is
`region' (the default) for an active region, or `defun' for the defun at
point.  The prompt names the buffer location and the text's 1-based line
and column range, and says where the text came from."
  (save-excursion
    (concat
     (if instructions (concat instructions "\n\n---\n") "")
     (if (eq kind 'defun)
         "The current defun:\n\n"
       "The user selected the following text:\n\n")
     (format "%s:%d:%d-%d:%d:\n\n"
             (herdr--buffer-location)
             (line-number-at-pos beg t)
             (progn (goto-char beg) (1+ (current-column)))
             (line-number-at-pos end t)
             (progn (goto-char end) (1+ (current-column))))
     (if full
         (buffer-substring-no-properties beg end)
       (herdr--region-text beg end))
     (herdr--buffer-read-hint))))

;;;###autoload
(defun herdr-ask-region (agent beg end &optional instructions full kind)
  "Send the region between BEG and END to Herdr AGENT.
BEG and END may be positions or markers.  Marker endpoints track edits
made while reading instructions.  Include the buffer's file name, or an
explicit \"Emacs buffer\" label with its buffer name when it does not
visit a file, and the region's line and column range.  Regions longer
than `herdr-region-max-length' are summarized unless optional argument
FULL is non-nil.  If INSTRUCTIONS is non-nil, prepend it to the prompt.
KIND is `region' (the default) for an active region, or `defun' for the
defun at point; it only affects the prompt's wording.
Interactively, always read INSTRUCTIONS and use the prefix argument as
FULL."
  (interactive (list (herdr-get-project-agent)
		     (copy-marker (region-beginning))
		     (copy-marker (region-end) t)
		     (herdr--read-prompt)
		     current-prefix-arg))
  (let ((beg (copy-marker beg))
	(end (copy-marker end t)))
    (when (equal instructions "")
      (setq instructions nil))
    (herdr-ask agent (herdr--ask-region-prompt beg end instructions full kind))))

(defun herdr--region-or-defun-bounds ()
  "Return the active region or defun bounds as a cons of markers, or nil.
Return nil when there is neither an active region nor a defun at point."
  (when-let* ((bounds (if (use-region-p)
                          (cons (region-beginning) (region-end))
                        (bounds-of-thing-at-point 'defun))))
    (cons (copy-marker (car bounds))
          (copy-marker (cdr bounds) t))))

;;;###autoload
(defun herdr-ask-dwim ()
  "Send the active region or current defun to a Herdr agent.
When there is neither an active region nor a defun, ask the agent a
question instead, like `herdr-ask'.  Prompt for instructions; an empty
response means no instructions.  With a prefix argument, send the
selected text in full regardless of `herdr-region-max-length'."
  (interactive)
  (let* ((region-active (use-region-p))
	 (bounds (herdr--region-or-defun-bounds))
	 (agent (herdr-get-project-agent))
	 (instructions (herdr--read-prompt)))
    (if bounds
	(herdr-ask-region agent (car bounds) (cdr bounds)
			  (unless (equal instructions "") instructions)
			  current-prefix-arg
			  (if region-active 'region 'defun))
      (herdr-ask agent instructions
		 (and current-prefix-arg
		      (prefix-numeric-value current-prefix-arg))))))

;;;###autoload
(defun herdr-send-region (agent beg end)
  "Send the region between BEG and END to Herdr AGENT verbatim.
Unlike `herdr-ask-region', send only the selected text: no file
reference, no instructions, and no read hint."
  (interactive (list (herdr-get-project-agent)
		     (copy-marker (region-beginning))
		     (copy-marker (region-end) t)))
  (herdr-ask agent (buffer-substring-no-properties beg end)))

;;;###autoload
(defun herdr-send-dwim ()
  "Send the active region or current defun to a Herdr agent verbatim.
Unlike `herdr-ask-dwim', send only the selected text: no file
reference and no instructions."
  (interactive)
  (let ((bounds (herdr--region-or-defun-bounds)))
    (unless bounds
      (user-error "No active region or defun at point"))
    (herdr-send-region (herdr-get-project-agent)
		       (car bounds) (cdr bounds))))

;;;###autoload
(defun herdr-send-buffer ()
  "Send the current buffer to a Herdr agent verbatim.
Unlike `herdr-ask-region', send only the buffer text: no file
reference and no instructions."
  (interactive)
  (herdr-send-region (herdr-get-project-agent)
		     (point-min) (point-max)))

(provide 'herdr)

;;; herdr.el ends here
