;;; message-ring.el --- Save echo messages in a ring -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "31.0.50"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; This package saves all Emacs echo area messages in a fixed-size ring buffer,
;; letting you quickly retrieve and copy transient messages that would otherwise
;; scroll out of view or disappear entirely.

;; Enable the global minor mode `message-ring-mode' to automatically collect all
;; echo-area messages into the ring.

;; The main interactive command is `copy-message':
;;
;; - `M-x copy-message' (no prefix argument): copies the most recent message
;;   directly to the kill ring.
;; - `C-u M-x copy-message': prompts you to select any recent message from the
;;   completion list before copying it to the kill ring.

;; Messages matching any of the regular expressions in `message-ring-inhibit-regexps'
;; are automatically excluded from the ring, to avoid cluttering it with unimportant
;; transient messages.

;;; Code:


(require 'ring)

;;;; Echo message

(defvar message-ring (make-ring 128)
  "Ring buffer to store recent messages.")

(defvar message-ring-insert t
  "When non-nil, store messages in `message-ring'.
When nil, messages are not stored in the ring buffer.")

(defvar message-ring-inhibit-regexps
  (list "Keypad: "
	" is undefined\\'"
	"\\.\\{3\\}\\(?:done\\)?\\'")
  "List of regexps matching messages that should not be stored.
Messages matching any regexp in this list will be excluded from the ring.")

(defvar message-ring-insert-functions
  '(message-ring-ignore-keystroke
    message-ring-inhibit
    message-ring-insert)
  "List of functions to process messages before insertion.
Each function is called with the message string and should return:
- A string to continue processing with the next function
- nil to stop processing and discard the message
- Any other value to stop processing")

(defun message-ring-ignore-keystroke (message)
  "Filter out incomplete keystroke messages from MESSAGE.
If MESSAGE ends with '-' (indicating an incomplete key sequence),
attempt to validate it as a key. Return valid key or original MESSAGE."
  (let ((msg message))
    (when (string-suffix-p "-" msg)
      (setq msg (substring msg 0 -1)))
    (or (ignore-errors (key-valid-p msg))
	message)))

(defun message-ring-inhibit (message)
  "Check if MESSAGE should be inhibited from ring storage.
Return MESSAGE if it should be stored, nil if it matches any pattern
in `message-ring-inhibit-regexps' and should be discarded."
  (or (and (consp message-ring-inhibit-regexps)
	   (string-match-p (mapconcat #'identity message-ring-inhibit-regexps "\\|")
			   message))
      message))

(defun message-ring-insert (message)
  "Insert MESSAGE into `message-ring' buffer.
This is the final function in the processing chain that actually
stores the message in the ring buffer."
  (ring-remove+insert+extend message-ring message)
  t)

(defun message-insert-ring (message)
  "Insert MESSAGE into the message ring buffer when enabled.
Process MESSAGE through `message-ring-insert-functions' when
`message-ring-insert' is non-nil.  Each function can transform or
filter the message.  Return MESSAGE unchanged.

This function is designed to be added to `set-message-functions'."
  (prog1 message
    (when message-ring-insert
      (named-let loop
	  ((fns message-ring-insert-functions))
	(unless (null fns)
	  (let* ((fn (car fns))
		 (result (funcall fn message)))
	    (when (or (and (stringp result) (setq message result))
		      (null result))
	      (loop (cdr fns)))))))))

(defvar-keymap message-ring-mode-map
  "C-M-S-w" #'message-ring-copy-message)

;;;###autoload
(define-minor-mode message-ring-mode
  "Global minor mode to save echo area messages to a ring buffer.

When enabled, all echo area messages are saved to `message-ring',
allowing you to retrieve transient messages later using `copy-message'."
  :global t
  :lighter ""
  :keymap message-ring-mode-map
  :group 'message-ring
  (if message-ring-mode
      (add-hook 'set-message-functions #'message-insert-ring)
    (remove-hook 'set-message-functions #'message-insert-ring)))

(defun message-ring-read (prompt)
  "Read a message from the ring with PROMPT using completion.
Return the selected message string from `message-ring'."
  (let* ((messages (ring-elements message-ring))
	 (messages (mapcar #'substring-no-properties messages))
	 (metadata `((category . string)
		     (display-sort-function . ,#'identity)
		     (cycle-sort-function . ,#'identity)))
	 (table (completion-table-with-metadata messages metadata)))
    (completing-read prompt table nil t)))

(defun message-ring-copy-message (&optional arg)
  "Copy a message from the message ring to the kill ring.
With prefix argument ARG, prompt for a message from the history to copy.
Otherwise copy the most recent message (index 0)."
  (interactive "P")
  (when (ring-empty-p message-ring)
    (user-error "Message ring is empty"))
  (let* ((msg (if arg
		  (message-ring-read "Message to copy: ")
		(ring-ref message-ring 0)))
	 ;; Prevent this operation from adding to message history
	 (message-ring-insert nil)
	 (message-log-max nil))
    (setq msg (substring-no-properties msg))
    (kill-new msg)
    (message "Message copied to kill-ring: %s"
	     (propertize msg 'face 'font-lock-string-face))))

(defalias 'copy-message #'message-ring-copy-message)

(provide 'message-ring)
;;; message-ring.el ends here
