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
(require 'json)

;;; Models

(setopt gptel-default-mode 'org-mode)

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

(declare-function magit-commit-argument "magit-commit.el")

;;;###autoload
(defun +gptel-commit-staged (&optional args)
  "Generate a commit message for staged changes using LLM, then open for editing.

Queries LLM with the current git status, staged diff, and recent git log
to generate an appropriate commit message. Then opens the generated message
in the git editor for final editing before committing."
  (interactive (list (magit-commit-arguments)))
  (with-temp-buffer
    (let ((gptel-backend gptel-backend)
	  (gptel-model gptel-model)
	  (dir default-directory)
	  gptel-use-context
	  gptel-use-tools)
      (insert "<git-status>")
      (unless (zerop (magit-process-git t "status"))
	(error "Failed to run git status"))
      (insert "</git-status>\n")
      (insert "<git-diff-staged>")
      (unless (zerop (magit-process-git t "diff" "--cached" "--no-textconv"))
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

(defvar log-edit-listfun)
(defvar log-edit-diff-function)

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

;;;###autoload
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

;;;###autoload
(progn
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
      (advice-add command :around advice-symbol))))

;;;###autoload
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper for async HTTP requests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
and CALLBACK will be called with a timeout error.  If TIMEOUT is nil,
`gptel-tools-default-timeout' will be used.

Returns the process object for the URL retrieval."
  (let* ((timer nil)
	 (process nil)
         (cb (lambda (result)
               (when callback
                 (unwind-protect
                     (funcall callback result)
                   (when (timerp timer)
                     (cancel-timer timer)
                     (setq timer nil))
                   (when (processp process)
                     (delete-process process)
                     (setq process nil))
                   (setq callback nil))))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jira tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar gptel-tools-jira-host nil)

(defun gptel-tools--get-jira-token ()
  (cl-assert gptel-tools-jira-host)
  (auth-info-password
   (car
    (or (auth-source-search
	 :max 1
	 :host gptel-tools-jira-host)
	(error "No authinfo for %s" gptel-tools-jira-host)))) )

(defun gptel-tools--jira-request-async (callback url error-label)
  "Make an authenticated Jira API request asynchronously.
CALLBACK is called with the parsed JSON result or an error plist.
URL is the full API endpoint.
ERROR-LABEL is used in error messages."
  (condition-case err
      (let* ((token (gptel-tools--get-jira-token))
	     (url-request-method "GET")
	     (url-request-extra-headers
	      `(("authorization" . ,(format "Bearer %s" token))
		("accept" . "application/json"))))
	(gptel-tools--url-retrieve
	 (lambda (status)
	   (if (eq (car-safe status) :error)
	       (funcall callback status)
	     (condition-case err1
		 (let* ((json-object-type 'plist)
			(result (progn
				  (goto-char (point-min))
				  (search-forward "\n\n" nil t)
				  (json-parse-buffer))))
		   (kill-buffer (current-buffer))
		   (funcall callback result))
	       (error
		(kill-buffer (current-buffer))
		(funcall callback (list :error error-label
					      :internal-error (gptel--to-string err1)))))))
	 url))
    (error (funcall callback (list :error error-label
				   :internal-error (gptel--to-string err))))))

(defun gptel-tools--search-jira-issues-async (callback jql max-results)
  (gptel-tools--jira-request-async
   callback
   (format "https://%s/rest/api/2/search?jql=%s&maxResults=%s"
	   gptel-tools-jira-host
	   (url-hexify-string jql)
	   max-results)
   "Failed to search issues"))

(defun gptel-tools--get-jira-issue-async (callback issue-id)
  (gptel-tools--jira-request-async
   callback
   (format "https://%s/rest/api/2/issue/%s"
	   gptel-tools-jira-host
	   issue-id)
   "Failed to get issue"))

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
		     :description "Maximum number of results to return. \
The maximum allowed value is 20."))
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

(provide 'gptel-config)
;;; gptel-config.el ends here
