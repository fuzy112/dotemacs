;;; gptel-config.el --- Configuration for gptel AI assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026 Zhengyi Fu <i@fuzy.me>

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
  :models '((kimi-k2.5
	     :description "Kimi's most versatile model to date"
	     :capabilities (media tool-use json)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 256
	     :input-cost 0.10		; 0.60 if cache miss
	     :output-cost 3.00)
	    (kimi-k2-thinking
	     :description "The Kimi reasoning model"
	     :capabilities (reasoning media)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 256
	     :input-cost 0.15		;  0.60 if cache miss
	     :output-cost 2.50)
	    (kimi-k2-thinking-turbo
	     :description "The Kimi reasoning model"
	     :capabilities (reasoning media)
	     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
	     :context-window 256
	     :input-cost 0.15		; 1.15 if cache miss
	     :output-cost 8.00)
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
	     :input-cost 0.10		; 1.15 if cache miss
	     :output-cost 8.00)
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

(gptel-make-openai "Kimi Code"
  :host "api.kimi.com"
  :endpoint "/coding/v1/chat/completions"
  :header (lambda () (when-let* ((key (gptel--get-api-key)))
		  `(("Authorization" . ,(concat "Bearer " key))
		    ;; https://www.reddit.com/r/kimi/comments/1p9b6mc/accessing_the_kimi_for_coding_api/
		    ("User-Agent" . "claude-code/1.0")
		    ("X-Client-Name" . "claude-code"))))
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models '((kimi-for-coding
	     :description "Kimi Code - specialized coding model"
	     :capabilities (tool-use json)
	     :context-window 256)
	    (kimi-k2-thinking
	     :description "Kimi K2 Thinking - reasoning model (undocumented)"
	     :capabilities (reasoning tool-use json)
	     :context-window 256)))

(gptel-make-openai "Volcengine Coding"
  :host "ark.cn-beijing.volces.com"
  :endpoint "/api/coding/v3/chat/completions"
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models '((ark-code-latest
	     :description "Ark Code Latest"
	     :capabilities (reasoning tool-use json)
	     :context-window 256)
	    (deepseek-v3.2
	     :description "DeepSeek V3.2"
	     :capabilities (reasoning tool-use json)
	     :context-window 128)
	    (doubao-seed-code
	     :description "Doubao Seed Code"
	     :capabilities (reasoning tool-use json)
	     :context-window 256)
	    (glm-4.7
	     :description "GLM 4.7"
	     :capabilities (reasoning tool-use json)
	     :context-window 200)
	    (kimi-k2-thinking
	     :description "Kimi K2 thinking"
	     :capabilities (reasoning tool-use json)
	     :context-window 256)
	    (kimi-k2.5
	     :description "Kimi K2.5"
	     :capabilities (reasoning tool-use json)
	     :context-window 256)))

(gptel-make-deepseek "DeepSeek"
  :stream t
  :key #'gptel-api-key-from-auth-source)

(gptel-make-kagi "Kagi"
  :key #'gptel-api-key-from-auth-source)

(gptel-make-gh-copilot "Copilot")

(setq-default gptel-backend (gptel-get-backend "Volcengine Coding"))
(setq-default gptel-model 'ark-code-latest)


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

;;;###autoload
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

;;;; Custom Directives

(setf (alist-get 'language-learning gptel-directives)
      "You are my personal language tutor.

1.  Detect the language of the text I provide.
2.  Transliterate every non-Latin sentence into Latin script (or IPA if more appropriate).
3.  Give a faithful, natural-sounding Chinese (zh-CN) translation.
4.  Below the translation, list every word that is not obvious to an upper-beginner, in order of appearance.
   For each word show:
   - dictionary form (lemma)
   - pronunciation
   - part of speech
   - brief English or Simplified-Chinese definition
   - the inflection or derivation that appears in the text (if any)
   - the Chinese-character form (if the word is Sino-Korean, Japanese, Mongolian, Manchu, Tibetan, Uighur, or Vietnamese).
5.  Add a short note (≤100 characters) in Simplified Chinese or English—whichever you master better—on one striking grammar point, idiom, or cultural nuance.

Constraints I already satisfy
- German & Japanese: basic grammar, core vocab.
- Scripts: fluent in Hangul, Hiragana, Katakana, Tibetan alphabet; can slowly decode Greek & Cyrillic.
- All other scripts: treat as completely unknown.

Return nothing except the requested output; no meta-commentary, no bullet points beyond those specified.

# EXAMPLE 1

## Input

Elle s’est souvenu de l’avoir déjà vu.

## Output

### Transliteration

N/A

### Chinese Translation

她记得以前见过他。

### Vocabulary

- se souvenir /sə suvəniʁ/ 记得 Verb to remember
  Reflexive verb, 3rd sg past

- déjà /deʒa/ 已经 Adverb already

- voir /vwaʁ/ 看见 Verb to see
  Infinitive after preposition

### Note

过去时用être助动词，过去分词需与宾语前置配合。

# EXAMPLE 2

## Input

그는 한국어를 배우느라 고생이 많았다.

## Output

### Transliteration

Geuneun hangugeo-reul baeuneura gosaeng-i manatda.

### Chinese Translation

他学韩语吃了不少苦。

### Vocabulary

- 그는 /ɡɯnɯn/ 他 Topic he

- 한국어(韓國語) /han.ɡu.ɡʌ/ 韩语 Noun Korean language

- 배우느라 /pɛ.u.nɯ.ɾa/ 为了学…… Connective because of learning

- 고생(苦生) /ko.sɛŋ/ 辛苦 Noun hardship

- 많았다 /ma.nat̚.t͈a/ 很多 Adjective-past was much

### Note

“느라”表目的兼原因，后句多负面结果.
")

(setf (alist-get 'commit gptel-directives)
      "You are a large language model and an experienced software developer.
You will receive diffs and optional context from Magit, Emacs' Git interface.
Your task is to generate a clean, concise yet informative commit message for the changes.
Focus exclusively on staged changes; omit any mention of unstaged or untracked changes.
If previous Git commit history is provided, align your message with the existing project conventions:
  - Follow existing formatting, such as bullet points for multiple changes
  - Include descriptions of purpose and implementation details if that is the existing convention
  - Match the existing style if the project uses GNU ChangeLog format
  - Preserve any existing markdown usage
Output only the commit message, with no extra explanation or surrounding markup.")

;;;###autoload
(defun +gptel-language-tutor-primary-selection ()
  "Send the X primary selection to a full-frame GPT buffer configured for language tutoring."
  (interactive)
  (let ((buffer (gptel " *tutor*")))
    (pop-to-buffer buffer '((display-buffer-full-frame)))
    (with-current-buffer buffer
      (setq-local gptel-backend (gptel-get-backend "Moonshot"))
      (setq-local gptel-model 'kimi-k2-0905-preview)
      (setq-local gptel-use-tools nil)
      (setq-local gptel--system-message (alist-get 'language-learning gptel-directives))
      (goto-char (point-max))
      (call-process "wl-paste" nil t t "-p")
      (newline)
      (gptel-send))))

;;;###autoload
(defun +gptel-commit-staged (&optional args)
  "Generate a commit message for staged changes using LLM, then open for editing.

Queries LLM with the current git status, staged diff, and recent git log
to generate an appropriate commit message. Then opens the generated message
in the git editor for final editing before committing."
  (interactive (list (magit-commit-arguments)))
  (require 'magit)
  (with-temp-buffer
    (let ((gptel-backend (gptel-get-backend "Volcengine Coding"))
	  (gptel-model 'ark-code-latest)
	  (dir default-directory)
	  gptel-use-context
	  gptel-use-tools)
      (insert "<git-status>")
      (unless (zerop (magit-process-git t "status"))
	(error "Failed to run git status"))
      (insert "</git-status>\n")
      (insert "<git-diff-staged>")
      (unless (zerop (magit-process-git t "diff" "--cached"))
	(error "Failed to run git diff --cached"))
      (insert "</git-diff-staged>\n")
      (insert "<git-log>")
      (unless (zerop (magit-process-git t "log" "-n10" "--stat"))
	(error "Failed to run git log"))
      (insert "</git-log>\n")
      (gptel-request
	  (buffer-string)
	:system (alist-get 'commit gptel-directives)
	:callback (lambda (response _info)
		    (if (stringp response)
			(let ((default-directory dir))
			  (apply #'magit-run-git-with-editor "commit" "-m" response "--edit" args))
		      (message "Failed to query LLM")))))))

;;;###autoload
(with-eval-after-load 'magit-commit
  (transient-append-suffix 'magit-commit "c"
    '("L" "Commit with AI-generated message" +gptel-commit-staged)))

;;;###autoload
(defun gptel-log-edit-generate-commit-message ()
  "Use GPTel to generate a commit message based on the current diff."
  (interactive)
  (if-let* ((staged-files (funcall log-edit-listfun))
	    (diff-content (save-window-excursion
			    (funcall log-edit-diff-function)
			    (buffer-string)))
	    (original-commit (buffer-string))
	    (system-prompt (alist-get 'commit gptel-directives)))
      (progn
	(erase-buffer)
	(let (gptel-use-tools gptel-use-context)
	  (gptel-request
	      (format "<staged-files>%s</staged-files>
<git-diff>%s</git-diff>
<original-commit-message>%s</original-commit-message>
"
		      (string-join staged-files "\n")
		      diff-content
		      original-commit)
	    :system system-prompt
	    :stream t)))
    (user-error "Failed to prepare diff context for commit message generation")))

;;;###autoload
(with-eval-after-load 'log-edit
  (keymap-set log-edit-mode-map "C-c C-S-m" #'gptel-log-edit-generate-commit-message))

(defun gptel-context-at-point (&optional context-lines)
  "Return context around point with line numbers, marking the current line.
Optional CONTEXT-LINES (default 10) specifies how many lines to include
above and below the current line."
  (interactive "p")
  (setq context-lines (if (numberp context-lines)
			  (max 1 context-lines)
			25))
  (let* ((current-lines (if (use-region-p)
			    (let ((first (save-excursion
						(goto-char (use-region-beginning))
						(line-number-at-pos)))
				  (last (save-excursion
					      (goto-char (use-region-end))
					      (line-number-at-pos))))
			      (cl-loop for i from first to last collect i))
			  (list (line-number-at-pos))))
	 (start-line (max 1 (- (car current-lines) context-lines)))
	 (end-line (+ (car (last current-lines)) context-lines))
	 (beg (save-excursion (goto-line start-line) (line-beginning-position)))
	 (end (save-excursion (goto-line end-line) (line-end-position)))
	 (text (buffer-substring-no-properties beg end))
	 (lines (string-lines text)))
    (cl-loop for line in lines
	     for i from start-line to end-line
	     concat (format "%s%d: %s\n"
			    (if (memql i current-lines) "=> " "")
			    i
			    line)
	     into result
	     finally return (if (called-interactively-p 'any)
				(message "%s" (setq result (string-trim result)))
			      result))))

(cl-defstruct gptel-minibuffer-spinner
  "A spinning indicator in the minibuffer prompt."
  timer overlay)

(defvar gptel-minibuffer-spinner-style-alist
  '((dots . ["." ".." "..."])
    (slashes . [ "/" "-" "\\" "|"])
    (braille . ["⡀" "⡄" "⡆" "⡇" "⣇" "⣧" "⣷" "⣿" "⣿" "⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"])
    (line . ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂"])
    (pulse . ["●" "◐" "○" "◔"])
    (arrow . ["←" "↖" "↑" "↗" "→" "↘" "↓" "↙"])
    (bounce . ["▪▪▪" "■▪▪" "▪■▪" "▪▪■" "▪■▪" "■▪▪"])
    (wave . ["▹▹▹▹▹" "▸▹▹▹▹" "▹▸▹▹▹" "▹▹▸▹▹" "▹▹▹▸▹"])
    (moon . ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])))

(defvar gptel-minibuffer-spinner-style 'braille)
(defface gptel-minibuffer-spinner
  '((t :foreground "cyan"))
  "Face for gptel-minibuffer-spinner.")

(defun gptel-minibuffer-spinner-start (&optional style)
  "Start a spinning indicator in the current minibuffer.
Return a `gptel-minibuffer-spinner' structure.  Stop it with
`gptel-minibuffer-spinner-stop'."
  (let* ((overlay (make-overlay (point) (point) nil nil t))
	 (count 0)
	 (data (alist-get (or style gptel-minibuffer-spinner-style)
			  gptel-minibuffer-spinner-style-alist))
	 (update (lambda ()
		   (move-overlay overlay
				 (1- (minibuffer-prompt-end))
				 (minibuffer-prompt-end))
		   (overlay-put overlay 'face 'gptel-minibuffer-spinner)
		   (overlay-put overlay 'display
				(propertize (format "%s" (aref data count))))
		   (setq count (mod (1+ count) (length data)))))
	 (timer (run-at-time 0.5 0.5 update)))
    (make-gptel-minibuffer-spinner
     :timer timer
     :overlay overlay)))

(defun gptel-minibuffer-spinner-stop (spinner)
  "Stop and clean up SPINNER, a `gptel-minibuffer-spinner'."
  (when-let* ((timer (gptel-minibuffer-spinner-timer spinner))
	      ((timerp timer)))
    (cancel-timer timer))
  (delete-overlay (gptel-minibuffer-spinner-overlay spinner)))

(defun gptel-request-minibuffer-input (&rest args)
  "Start a streaming gptel request for minibuffer input.
ARGS are passed to `gptel-request', with an updated streaming
callback that inserts the response into the minibuffer."
  (cl-assert (minibufferp))
  (let* ((buffer (current-buffer))
	 (state 'stopped)
	 (spinner (gptel-minibuffer-spinner-start))
	 (cleanup-function (lambda ()
			     (gptel-minibuffer-spinner-stop spinner)
			     (when (eq state 'running)
			       (gptel-abort buffer)
			       (setq state 'stopped))))
	 gptel-use-context
	 gptel-use-tools)
    (add-hook 'post-self-insert-hook cleanup-function nil t)
    (add-hook 'minibuffer-exit-hook cleanup-function nil t)
    (condition-case err
	(progn
	  (apply #'gptel-request
		 (append args
			 (list
			  :stream t
			  :callback
			  (lambda (response _info)
			    (cond ((eq response t)
				   (setq state 'stopped)
				   (gptel-minibuffer-spinner-stop spinner))
				  ((stringp response)
				   (with-current-buffer buffer
				     (goto-char (point-max))
				     (insert response))))))))
	  (setq state 'running))
      (error
       (funcall cleanup-function)
       (signal (car err) (cdr err))))))

(cl-defun gptel-autosuggest-define
    (command &key system context match-prompt (name 'autosuggest) backend model)
  "Add GPTel-based auto-suggestion functionality to COMMAND.
COMMAND is an interactive function symbol.  SYSTEM is the system prompt
string.  CONTEXT can be a string or a function of no arguments that
returns a context string.  MATCH-PROMPT is an optional regexp: if
provided, suggestions are only enabled when the minibuffer prompt
matches it.  BACKEND is the gptel backend to use, either a backend
object or a string name; defaults to `gptel-backend'.  MODEL is the
model to use; defaults to `gptel-model'.  This function uses advice to
modify COMMAND.  If NAME is non-nil, the advice is named
`COMMAND@NAME'."
  (declare (indent 1))
  (let ((advice-symbol (intern (format "%s@%s" command name))))
    (fset advice-symbol (lambda (orig-fn &rest args)
			  (let* ((computed-context
				  (cond
				   ((functionp context) (funcall context))
				   ((stringp context) context)
				   (t (error "Invalid context: expected string or function"))))
				 (computed-system system)
				 (gptel-backend (cond
						 ((stringp backend)
						  (gptel-get-backend backend))
						 (backend backend)
						 (t gptel-backend)))
				 (gptel-model (or model gptel-model)))
			    (minibuffer-with-setup-hook
				(lambda ()
				  (when (or (not match-prompt)
					    (string-match-p match-prompt (minibuffer-prompt)))
				    (gptel-request-minibuffer-input computed-context :system computed-system)))
			      (apply orig-fn args)))))
    (advice-add command :around advice-symbol)))

(gptel-autosuggest-define 'bookmark-set
  :system
  "You are helping the user create a clear, descriptive bookmark name for their current position in a code/text file.

The selected lines are marked with \"=>\".

Follow these rules when creating the bookmark name:
1. Keep it 3 to 8 words long, concise but informative
2. Start with the file or component name to give it context
3. Clearly describe what this position in the file is for or what it contains
4. Use plain text with spaces, no quotes, no extra punctuation or explanations
5. Only output the bookmark name itself, nothing else

Example output: auth-service password validation function"
  :context (lambda ()
	     (require 'which-func)
	     (let* ((bookmark-point (if (use-region-p) (region-beginning) (point)))
		    (file-name (buffer-file-name))
		    (buffer-name (buffer-name))
		    (context (gptel-context-at-point))
		    (defun-name (which-function)))
	       (format "<input>
<file_path>%s</file_path>
<buffer_name>%s</buffer_name>
<scope>%s</scope>
<context>
%s
</context>
</input>"
		       file-name
		       buffer-name
		       defun-name
		       context)))
  :match-prompt "Set bookmark named"
  :backend "Volcengine Coding"
  :model 'ark-code-latest)

(provide 'gptel-config)
;;; gptel-config.el ends here
