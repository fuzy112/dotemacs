;;; telega-completion.el --- Native completion at point for telega  -*- lexical-binding:t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/.

;;; Commentary:

;; Completion at point functions (CAPF) to be used in telega:
;;   `telega-completion-emoji' to complete emojis
;;   `telega-completion-telegram-emoji' to complete Telegram custom emojis
;;   `telega-completion-username' to complete usernames
;;   `telega-completion-hashtag' to complete hashtags
;;   `telega-completion-botcmd' to complete bot commands
;;   `telega-completion-quick-reply' to complete quick reply shortcuts
;;   `telega-completion-markdown-precode' to complete language names for code blocks
;;
;; Usage:
;;
;;   Add `telega-completion-setup' to `telega-chat-mode-hook':
;;
;;     (add-hook 'telega-chat-mode-hook #'telega-completion-setup)
;;
;;   This enables all completion functions in telega chat buffers.
;;   Use TAB to trigger completion based on context.
;;
;;   To customize which completion functions to use, modify
;;   `telega-completion-at-point-functions' before calling setup.
;;   List them in order of execution; the first function has the
;;   highest priority:
;;
;;     (setq telega-completion-at-point-functions
;;           '(telega-completion-username
;;             telega-completion-hashtag
;;             telega-completion-emoji))
;;     (add-hook 'telega-chat-mode-hook #'telega-completion-setup)
;;
;;   For better Corfu integration with automatic prefix triggering,
;;   use `cape-capf-super' to combine related completions and set
;;   `corfu-auto-trigger' to the trigger characters:
;;
;;     (require 'cape)
;;     (setq telega-completion-at-point-functions
;;           (list
;;            (cape-capf-super #'telega-completion-emoji
;;                             #'telega-completion-telegram-emoji)
;;            #'telega-completion-username
;;            #'telega-completion-hashtag
;;            (cape-capf-super #'telega-completion-botcmd
;;                             #'telega-completion-quick-reply)
;;            #'telega-completion-markdown-precode))
;;
;;     (add-hook 'telega-chat-mode-hook
;;               (lambda ()
;;                 (telega-completion-setup)
;;                 (setq-local corfu-auto t
;;                             corfu-quit-no-match nil
;;                             corfu-auto-trigger "@#:/")
;;                 (corfu-mode)))
;;
;; Integration:
;;
;;   These CAPFs work with the default Emacs completion UI, as well as
;;   alternative completion UIs such as Company or Corfu.

;;; Code:

(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-user)
(require 'telega-chat)
(require 'telega-emoji)


(declare-function telega-chat-admin-get "telega-chat" (chat user))
(declare-function telega-chat--info "telega-chat" (chat &optional locally-p))
(declare-function telega--full-info "telega-info" (tlobj &optional _callback))
(declare-function telega-string-as-markup "telega-markup" (text markup markup-func))
(declare-function telega-ins--content-one-line "telega-ins" (msg))
(declare-function telega-quick-reply-by-name "telega-quick-reply" (name))
(declare-function telega-ins--msg-sender "telega-ins" (msg-sender &rest args))
(declare-function telega-msg-sender-avatar-image-one-line "telega-util" (sender))
(declare-function telega-filter-chats "telega" (chats filter))
(declare-function telega-user--by-username "telega-user" (username))

;;;; Customization

(defgroup telega-completion nil
  "Completion support for telega."
  :group 'telega
  :prefix "telega-completion-")

(defcustom telega-completion-at-point-functions
  '(telega-completion-emoji
    telega-completion-telegram-emoji
    telega-completion-username
    telega-completion-hashtag
    telega-completion-botcmd
    telega-completion-quick-reply
    telega-completion-markdown-precode)
  "List of completion functions to use in telega chat buffers.
Each function should be a completion-at-point function (CAPF).
Customize this to enable only the completion types you want."
  :type '(repeat function)
  :group 'telega-completion)

(defcustom telega-completion-username-prefer-name
  '(username first-name last-name)
  "Preferred name formats for username completion.
List of symbols in order of preference: `username', `first-name', `last-name'.
The first available format is used when inserting a mention."
  :type '(repeat (choice (const username)
			 (const first_name)
			 (const last_name)))
  :group 'telega-completion)

(defcustom telega-completion-username-markup nil
  "Markup style for username mentions.
When non-nil, specifies the markup style to use for mentions.
Possible values: nil, `markdown1', `markdown2'.
When nil, use telega's native mention format."
  :type '(choice (const :tag "Native" nil)
		 (const markdown1)
		 (const markdown2))
  :group 'telega-completion)

(defcustom telega-completion-username-show-avatars t
  "If non-nil, show user avatars in username completion annotations."
  :type 'boolean
  :group 'telega-completion)

(defcustom telega-completion-username-complete-nonmember-for
  '(type bot)
  "Chat filter used to complete usernames from non-members.
Used when no matching chat members are found."
  :type 'telega-chat-temex
  :group 'telega-completion)

(defcustom telega-completion-username-sort-predicate
  #'telega-completion--username-sort-by-admin-and-online
  "Function to sort username completion candidates.
The function should accept two candidate strings and return non-nil if
the first should come before the second."
  :type '(choice (const :tag "Admins first, then online status"
			telega-completion--username-sort-by-admin-and-online)
		 (const :tag "Alphabetical" string<)
		 (const :tag "Recently contacted first"
			telega-completion--username-sort-by-recent)
		 (function :tag "Custom function"))
  :group 'telega-completion)

(defcustom telega-completion-emoji-sort-predicate
  #'string<
  "Function to sort emoji completion candidates.
The default is alphabetical order."
  :type '(choice (const :tag "Alphabetical" string<)
		 (const :tag "By length" length<)
		 (function :tag "Custom function"))
  :group 'telega-completion)

(defcustom telega-completion-hashtag-sort-predicate
  #'telega-completion--hashtag-sort-by-frequency
  "Function to sort hashtag completion candidates."
  :type '(choice (const :tag "By frequency/recency"
			telega-completion--hashtag-sort-by-frequency)
		 (const :tag "Alphabetical" string<)
		 (function :tag "Custom function"))
  :group 'telega-completion)

;;;; Helper Functions

(defun telega-completion--username-sort-by-admin-and-online (a b)
  "Sort predicate for username candidates A and B.
Sorts admins first, then by online status."
  (let* ((member-a (get-text-property 0 'telega-member a))
	 (member-b (get-text-property 0 'telega-member b))
	 (admin-a (and member-a
		       (telega-user-p member-a)
		       (telega-chat-admin-get telega-chatbuf--chat member-a)))
	 (admin-b (and member-b
		       (telega-user-p member-b)
		       (telega-chat-admin-get telega-chatbuf--chat member-b)))
	 (online-a (and member-a
			(eq 'userStatusOnline
			    (telega--tl-type (plist-get member-a :status)))))
	 (online-b (and member-b
			(eq 'userStatusOnline
			    (telega--tl-type (plist-get member-b :status))))))
    (cond
     ;; Admins come first
     ((and admin-a (not admin-b)) t)
     ((and admin-b (not admin-a)) nil)
     ;; Then online users
     ((and online-a (not online-b)) t)
     ((and online-b (not online-a)) nil)
     ;; Finally alphabetical
     (t (string< a b)))))

(defun telega-completion--username-sort-by-recent (a b)
  "Sort predicate for username candidates A and B.
Sorts by recently contacted (if available), then alphabetical."
  (let* ((member-a (get-text-property 0 'telega-member a))
	 (member-b (get-text-property 0 'telega-member b))
	 ;; Check if there's recent interaction data
	 ;; This is a placeholder - telega may not expose this directly
	 (has-interaction-a (plist-get member-a :telega-last-message-date))
	 (has-interaction-b (plist-get member-b :telega-last-message-date)))
    (cond
     ;; Users with recent interactions come first
     ((and has-interaction-a (not has-interaction-b)) t)
     ((and has-interaction-b (not has-interaction-a)) nil)
     ;; If both have interactions, sort by date (newer first)
     ((and has-interaction-a has-interaction-b)
      (> has-interaction-a has-interaction-b))
     ;; Default to alphabetical
     (t (string< a b)))))

(defun telega-completion--hashtag-sort-by-frequency (a b)
  "Sort predicate for hashtag candidates A and B.
Sorts by frequency (if available), then alphabetical.
NOTE: This is a simple implementation. For true frequency tracking,
a cache would need to be maintained."
  ;; For now, just do alphabetical
  ;; In the future, could track usage frequency per session
  (string< a b))

(defun telega-completion--emoji-sort (completions)
  "Sort COMPLETIONS for emoji completion.
Sorts by the actual emoji string associated with each candidate."
  (cl-sort completions telega-completion-emoji-sort-predicate
           :key (lambda (s) (or (get-text-property 0 'emoji s)
                           (alist-get s telega-emoji-alist nil nil #'string=)))))

(defun telega-completion--telegram-emoji-sort (completions)
  "Sort COMPLETIONS for Telegram emoji completion.
Sorts by the 'emoji text property."
  (cl-sort completions telega-completion-emoji-sort-predicate
           :key (lambda (s) (get-text-property 0 'emoji s))))

(defun telega-completion--username-sort (completions)
  "Sort COMPLETIONS for username completion.
Sorts using `telega-completion-username-sort-predicate'."
  (cl-sort completions telega-completion-username-sort-predicate))

(defun telega-completion--hashtag-sort (completions)
  "Sort COMPLETIONS for hashtag completion.
Sorts using `telega-completion-hashtag-sort-predicate'."
  (cl-sort completions telega-completion-hashtag-sort-predicate))

(defun telega-completion--dynamic-table (beg end fetcher)
  "Create dynamic completion table using FETCHER function.
BEG and END are the completion region boundaries.
FETCHER is a function that takes the current input string and returns
a list of completion candidates.

The returned table caches results and only refetches when the input changes."
  (let ((beg (copy-marker beg))
	(end (copy-marker end t))
	(last-input 'init)
	table)
    (lambda (str pred action)
      (unless (or (eq action 'metadata)
		  (eq (car-safe action) 'boundaries))
	(let ((new (buffer-substring-no-properties beg end)))
	  (when (and (not (string-empty-p new))
		     (or (eq last-input 'init)
			 (not (string= last-input new))))
	    (setq last-input new
		  table (funcall fetcher new)))
	  (complete-with-action action table str pred))))))

;;;; Emoji Completion

(defun telega-completion--emoji-annotation (s)
  "Return annotation for emoji S.
Shows the actual emoji character or image if available."
  (when-let* ((emoji (alist-get s telega-emoji-alist nil nil #'string=)))
    (concat " "
	    (cond
	     ;; Try to use telega's image display if available
	     ((and telega-emoji-use-images
		   (fboundp 'telega-emoji-create-svg))
	      (propertize "EE" 'display (telega-emoji-create-svg emoji)))
	     ;; Otherwise just show the emoji
	     (t emoji)))))

(defun telega-completion--emoji-exit-function (beg end)
  "Return exit function for built-in emoji completion.
BEG and END are the completion region boundaries."
  (let ((beg-marker (copy-marker beg))
	(end-marker (copy-marker end t)))
    (lambda (str status)
      (unless (eq status 'exact)
	(when-let* ((emoji (alist-get str telega-emoji-alist nil nil #'string=)))
	  (delete-region beg-marker end-marker)
	  (insert emoji))))))

;;;###autoload
(defun telega-completion-emoji (&optional interactive)
  "Complete emoji at point.
If INTERACTIVE is non-nil (called interactively), trigger completion
immediately.  Otherwise, return a completion data structure for
`completion-at-point-functions'."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'telega-completion-emoji)))
	(completion-at-point))
    (when-let* ((beg (and (looking-back ":[a-zA-Z0-9_+-]*:?" (pos-bol))
			  (match-beginning 0)))
		(end (point)))
      (telega-emoji-init)
      (list beg end telega-emoji-alist
	    :annotation-function #'telega-completion--emoji-annotation
	    :exit-function (telega-completion--emoji-exit-function beg end)
	    :company-kind (lambda (_) 'text)
	    :exclusive 'no
	    :category 'telega-emoji
	    :display-sort-function #'telega-completion--emoji-sort
	    :cycle-sort-function #'telega-completion--emoji-sort))))

;;;; Telegram Emoji Completion

(defun telega-completion--telegram-emoji-annotation (s)
  "Return annotation for Telegram emoji S.
Shows the actual emoji character or SVG image if configured."
  (when-let* ((emoji (get-text-property 0 'emoji s)))
    (concat " "
	    (cond
	     ;; Try to use telega's image display if available
	     ((and (boundp 'telega-emoji-use-images)
		   telega-emoji-use-images
		   (fboundp 'telega-emoji-create-svg))
	      (propertize "EE" 'display (telega-emoji-create-svg emoji)))
	     ;; Otherwise just show the emoji
	     (t emoji)))))

(defun telega-completion--telegram-emoji-exit-function (beg end)
  "Return exit function for Telegram emoji completion.
BEG and END are the completion region boundaries."
  (let ((beg-marker (copy-marker beg))
	(end-marker (copy-marker end t)))
    (lambda (str status)
      (unless (eq status 'exact)
	(when-let* ((emoji (get-text-property 0 'emoji str)))
	  (delete-region beg-marker end-marker)
	  (insert emoji))))))

(defun telega-completion--fetch-telegram-emoji-candidates (pattern)
  "Fetch Telegram emoji candidates for PATTERN synchronously."
  (mapcar (lambda (ek)
	    (propertize (concat ":" (telega-tl-str ek :keyword) ":")
			'emoji (telega-tl-str ek :emoji)))
	  (telega--searchEmojis
	   (string-remove-suffix ":" (string-remove-prefix ":" pattern))
	   nil nil nil)))

(defun telega-completion--telegram-emoji-table (beg end)
  "Create dynamic completion table for Telegram emoji.
BEG and END are the completion region boundaries."
  (telega-completion--dynamic-table
   beg end
   #'telega-completion--fetch-telegram-emoji-candidates))

;;;###autoload
(defun telega-completion-telegram-emoji (&optional interactive)
  "Complete Telegram (custom) emoji at point.
If INTERACTIVE is non-nil (called interactively), trigger completion
immediately.  Otherwise, return a completion data structure for
`completion-at-point-functions'."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'telega-completion-telegram-emoji)))
	(completion-at-point))
    (when-let* ((beg (and (looking-back ":[a-zA-Z0-9_+-]*" (pos-bol))
			  (match-beginning 0)))
		(end (point))
		(table (telega-completion--telegram-emoji-table beg end)))
      (list beg end table
	    :category 'telega-telegram-emoji
	    :annotation-function #'telega-completion--telegram-emoji-annotation
	    :exit-function (telega-completion--telegram-emoji-exit-function beg end)
	    :company-kind (lambda (_) 'text)
	    :exclusive 'no
	    :display-sort-function #'telega-completion--telegram-emoji-sort
	    :cycle-sort-function #'telega-completion--telegram-emoji-sort))))

;;;; Username Completion

(defun telega-completion--username-annotation (s)
  "Return annotation for username S.
Only show title if the user has a username (to avoid redundancy).
Add admin/owner badge if applicable.
If `telega-completion-username-show-avatars' is non-nil, also show user avatars."
  (when-let* ((member (or (get-text-property 0 'telega-member s)
			  (telega-user--by-username s))))
    (concat " "
	    (if telega-completion-username-show-avatars
		(telega-ins--as-string
		 (telega-ins--msg-sender member :with-avatar-p t))
	      (telega-msg-sender-title member))
	    (when-let* ((admin (and (telega-user-p member)
				    (telega-chat-admin-get telega-chatbuf--chat member))))
	      (propertize
	       (concat " ("
		       (or (telega-tl-str admin :custom_title)
			   (if (plist-get admin :is_owner)
			       (telega-i18n "lng_owner_badge")
			     (telega-i18n "lng_admin_badge")))
		       ")")
	       'face 'font-lock-comment-face)))))

(defun telega-completion--username-exit-function (beg end)
  "Return exit function for username completion.
BEG and END are markers for the completion region."
  (let ((beg-marker (copy-marker beg))
	(end-marker (copy-marker end t)))
    (lambda (str status)
      (unless (eq status 'exact)
	(when-let* ((member (get-text-property 0 'telega-member str))
		    (input (get-text-property 0 'telega-input str)))
	  (delete-region beg-marker end-marker)
	  (when (telega-user-p member)
	    (when-let* ((fmt-names telega-completion-username-prefer-name)
			(name (let ((tmp-name nil))
				(while (and fmt-names (not tmp-name))
				  (setq tmp-name (telega-user-title
						  member (car fmt-names) 'raw)
					fmt-names (cdr fmt-names)))
				tmp-name)))
	      (telega-ins
	       (cond ((string-prefix-p "@" name)
		      name)
		     ((member telega-completion-username-markup
			      '(markdown1 markdown2))
		      (telega-string-as-markup
			  (format "[%s](tg://user?id=%d)"
				  name (plist-get member :id))
			  telega-completion-username-markup
			  (cdr (assoc telega-completion-username-markup
				      telega-chat-markup-functions))))
		     (t
		      (propertize
		       name
		       :tl-entity-type (list :@type "textEntityTypeMentionName"
					     :user_id (plist-get member :id))
		       'face 'telega-entity-type-mention
		       'rear-nonsticky nil
		       'front-sticky nil))))))
	  (insert " ")
	  (let ((chatbuf-input (telega-chatbuf-input-string)))
	    (when (or (member chatbuf-input telega-known-inline-bots)
		      (member chatbuf-input telega--recent-inline-bots))
	      (telega-chatbuf-attach-inline-bot-query 'no-search))))))))

(defun telega-completion--fetch-usernames-candidates (input)
  "Fetch username candidates for INPUT string.
Returns a list of propertized strings with completion candidates,
combining chat members, inline bots, and non-member users from the
Main chat list.
Candidates are sorted using `telega-completion--username-sort'."
  (let* ((search-term (string-remove-prefix "@" input))
	 (filter (if (string-prefix-p "@@" input)
		     '(:@type "chatMembersFilterAdministrators")
		   (list :@type "chatMembersFilterMention"
			 :topic_id (telega-chatbuf--MessageTopic))))
	 (members (telega--searchChatMembers
		      telega-chatbuf--chat search-term filter)))
    (or
     (nconc
      (mapcar (lambda (member)
		(propertize
		 (or (telega-msg-sender-username member 'with-@)
		     (telega-msg-sender-title member))
		 'telega-member member
		 'telega-input input))
	      (cl-remove-if (telega-match-gen-predicate 'sender
			      '(or is-blocked (user is-deleted)))
			    members))
      (cl-remove-if-not (lambda (botname)
			  (string-prefix-p input botname 'ignore-case))
			(cl-union telega--recent-inline-bots
				  telega-known-inline-bots
				  :test #'string=)))
     ;; Non-member completion from Main chat list
     (unless (string-prefix-p "@@" input)
       (cl-remove-if-not
	(lambda (username)
	  (and username
	       (string-prefix-p input (concat "@" username) 'ignore-case)))
	(mapcar #'telega-msg-sender-username
		(telega-filter-chats (telega-chats-list)
		  telega-completion-username-complete-nonmember-for)))))))

(defun telega-completion--username-table (beg end)
  "Create dynamic completion table for usernames.
BEG and END are the completion region boundaries."
  (unless (derived-mode-p 'telega-chat-mode)
    (error "`telega-completion-username' can be used only in chat buffer"))
  (telega-completion--dynamic-table
   beg end
   #'telega-completion--fetch-usernames-candidates))

;;;###autoload
(defun telega-completion-username (&optional interactive)
  "Complete username at point.
If INTERACTIVE is non-nil (called interactively), trigger completion
immediately.  Otherwise, return a completion data structure for
`completion-at-point-functions'."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'telega-completion-username)))
	(completion-at-point))
    (when-let* ((prefix (and (or (looking-back "\\(@@\\)[^ \n\t]*" (pos-bol))
				 (looking-back "\\(@\\)[^ \n\t]*" (pos-bol)))
			     (match-string-no-properties 1)))
		(prefix-len (length prefix))
		(beg (match-beginning 0))
		(end (point))
		(table (telega-completion--username-table beg end)))
      (list (+ beg prefix-len) end table
	    :category 'telega-username
	    :annotation-function #'telega-completion--username-annotation
	    :exclusive 'no
	    :exit-function (telega-completion--username-exit-function beg end)
	    :display-sort-function #'telega-completion--username-sort
	    :cycle-sort-function #'telega-completion--username-sort)))))

;;;; Hashtag Completion

(defun telega-completion--hashtag-table (beg end)
  "Return completion table for hashtags.
BEG and END are markers for the completion region."
  (unless (derived-mode-p 'telega-chat-mode)
    (error "`telega-completion-hashtag' can be used only in chat buffer"))
  (let ((beg (copy-marker beg))
	(end (copy-marker end t))
	(input 'init)
	table)
    (lambda (str pred action)
      (unless (or (eq action 'metadata)
		  (eq (car-safe action) 'boundaries))
	(let ((new (buffer-substring-no-properties beg end)))
	  (when (and (or (eq input 'init)
			 (not (string= input new)))
		     (> (length new) 0))
	    (setq input new
		  table (mapcar (lambda (ht) (concat "#" ht))
				(telega--searchHashtags (substring new 1)))))
	  (complete-with-action action table str pred)))))

;;;###autoload
  (defun telega-completion-hashtag (&optional interactive)
    "Complete hashtag at point.
If INTERACTIVE is non-nil (called interactively), trigger completion
immediately.  Otherwise, return a completion data structure for
`completion-at-point-functions'."
    (interactive (list t))
    (if interactive
	(let ((completion-at-point-functions (list #'telega-completion-hashtag)))
	  (completion-at-point))
      (when-let* ((beg (and (not (bobp))
			    (= (char-before) ?#)
			    (save-excursion
			      (skip-chars-backward "#")
			      (point))))
		  (end (point))
		  (table (telega-completion--hashtag-table beg end)))
	(list beg end table
	      :category 'telega-hashtag
	      :company-kind (lambda (_) 'keyword)
	      :exclusive 'no
	      :exit-function (lambda (_str status)
			       (unless (eq status 'exact)
				 (insert " ")))
	      :display-sort-function #'telega-completion--hashtag-sort
	      :cycle-sort-function #'telega-completion--hashtag-sort)))))

;;;; Bot Command Completion

(defun telega-completion--bot-commands-list (bot-commands &optional suffix)
  "Convert BOT-COMMANDS to completion candidates.
Optional SUFFIX is appended to each command."
  (mapcar (lambda (bot-cmd)
	    (propertize (concat "/" (telega-tl-str bot-cmd :command) suffix)
			'telega-annotation
			(telega-ins--as-string
			 (telega-ins--with-attrs
			     (list :max (/ telega-chat-fill-column 2) :elide t)
			   (telega-ins (telega-tl-str bot-cmd :description))))))
	  bot-commands))

(defun telega-completion--fetch-bot-commands ()
  "Fetch list of bot commands for the current chat.
Returns a list of propertized strings with `telega-annotation' property
describing each command.
Handles both single bot chats and group chats with multiple bots."
  (when telega-chatbuf--chat
    (let* ((info (telega-chat--info telega-chatbuf--chat))
	   (telega-full-info-offline-p nil)
	   (full-info (when info (telega--full-info info))))
      (when full-info
	(if (telega-chatbuf-match-p '(type bot))
	    ;; Single bot chat
	    (telega-completion--bot-commands-list
	     (telega--tl-get full-info :bot_info :commands))
	  ;; Group chat with multiple bots
	  (apply #'nconc
		 (mapcar (lambda (bot-commands)
			   (telega-completion--bot-commands-list
			    (plist-get bot-commands :commands)
			    (let ((bot-user (telega-user-get
					     (plist-get bot-commands :bot_user_id))))
			      (telega-msg-sender-username bot-user 'with-@))))
			 (plist-get full-info :bot_commands))))))))

(defun telega-completion--botcmd-annotation (s)
  "Return annotation for bot command S."
  (when-let* ((annotation (get-text-property 0 'telega-annotation s)))
    (concat " " annotation)))

;;;###autoload
(defun telega-completion-botcmd (&optional interactive)
  "Complete bot command at point.
If INTERACTIVE is non-nil (called interactively), trigger completion
immediately.  Otherwise, return a completion data structure for
`completion-at-point-functions'."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'telega-completion-botcmd)))
	(completion-at-point))
    (when-let* ((beg (and (looking-back "/[^ ]*" (line-beginning-position))
			  (= telega-chatbuf--input-marker (match-beginning 0))
			  (match-beginning 0)))
		(end (point))
		(commands (telega-completion--fetch-bot-commands)))
      (list beg end commands
	    :category 'telega-botcmd
	    :annotation-function #'telega-completion--botcmd-annotation
	    :company-kind (lambda (_) 'function)
	    :exclusive 'no
	    :exit-function (lambda (_str status)
			     (unless (eq status 'exact)
			       (insert " ")))))))

;;;; Quick Reply Completion

(defun telega-completion--quick-reply-annotation (s)
  "Return annotation for quick reply shortcut S."
  (when-let* ((qr (telega-quick-reply-by-name (string-trim s "/")))
	      (msg (plist-get qr :first_message)))
    (concat " "
	    (telega-ins--as-string
	     (telega-ins--content-one-line msg)
	     (let ((nmessages (length (plist-get qr :messages))))
	       (when (> nmessages 1)
		 (telega-ins--with-face 'telega-shadow
		   (telega-ins " +" (telega-i18n "lng_forum_messages"
				      :count (1- nmessages))))))))))

(defun telega-completion--fetch-quick-replies ()
  "Fetch list of quick reply shortcuts.
Returns a list of propertized strings with completion candidates
for quick reply shortcuts."
  (mapcar (lambda (qr)
	    (propertize
	     (concat "/" (telega-tl-str qr :name))
	     'telega-annotation
	     (telega-ins--as-string
	      (telega-ins--content-one-line (plist-get qr :first_message)))))
	  telega--quick-replies))

;;;###autoload
(defun telega-completion-quick-reply (&optional interactive)
  "Complete quick reply shortcut at point.
If INTERACTIVE is non-nil (called interactively), trigger completion
immediately.  Otherwise, return a completion data structure for
`completion-at-point-functions'."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'telega-completion-quick-reply)))
	(completion-at-point))
    (when-let* (((telega-chatbuf-match-p '(type private)))
		(beg (and (looking-back "/[^ ]*" (line-beginning-position))
			  (= telega-chatbuf--input-marker (match-beginning 0))
			  (match-beginning 0)))
		(end (point))
		(shortcuts (telega-completion--fetch-quick-replies)))
      (list beg end shortcuts
	    :category 'telega-quick-reply
	    :annotation-function #'telega-completion--quick-reply-annotation
	    :company-kind (lambda (_) 'snippet)
	    :exclusive 'no))))

;;;; Markdown Precode Completion

(defun telega-completion--language-names ()
  "Return list of language names for code blocks sorted by usage."
  (let* ((all-buffers (buffer-list))
	 (modes
	  (seq-uniq (seq-filter #'symbolp (mapcar #'cdr auto-mode-alist))))
	 (indexed-modes
	  (mapcar (lambda (mode)
		    (cons mode (seq-count
				(lambda (buffer)
				  (eq (buffer-local-value 'major-mode buffer)
				      mode))
				all-buffers)))
		  modes))
	 (sorted-modes
	  (mapcar #'car (cl-sort indexed-modes #'> :key #'cdr))))
    (delq nil (mapcar (lambda (mode)
			(let ((mode-name (symbol-name mode)))
			  (when (string-suffix-p "-mode" mode-name)
			    (substring mode-name 0 -5))))
		      sorted-modes))))

(defun telega-completion--markdown-precode-exit-function (beg end)
  "Return exit function for markdown precode completion.
BEG and END are the completion region boundaries."
  (let ((beg-marker (copy-marker beg))
	(end-marker (copy-marker end t)))
    (lambda (str status)
      (unless (eq status 'exact)
	(delete-region beg-marker end-marker)
	(insert str)
	(if (save-excursion (re-search-forward "^```" nil 'noerror))
	    (forward-char)
	  (insert "\n")
	  (save-excursion (insert "\n```")))))))

;;;###autoload
(defun telega-completion-markdown-precode (&optional interactive)
  "Complete language name for markdown code block at point.
If INTERACTIVE is non-nil (called interactively), trigger completion
immediately.  Otherwise, return a completion data structure for
`completion-at-point-functions'."
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list #'telega-completion-markdown-precode)))
	(completion-at-point))
    (when-let* ((beg (and (looking-back "```\\([^\\t\\n ]*\\)" (line-beginning-position))
			  (match-beginning 1)))
		(end (match-end 1))
		(languages (telega-completion--language-names)))
      (list beg end languages
	    :category 'telega-markdown-precode
	    :company-kind (lambda (_) 'keyword)
	    :exclusive 'no
	    :exit-function (telega-completion--markdown-precode-exit-function beg end)))))

;;;; Integration

;;;###autoload
(defun telega-completion-setup ()
  "Set up completion for the current telega chat buffer."
  (dolist (capf (reverse telega-completion-at-point-functions))
    (add-hook 'completion-at-point-functions capf nil t)))

(provide 'telega-completion)
;;; telega-completion.el ends here
