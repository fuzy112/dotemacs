;;; capf-autosuggest-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from capf-autosuggest.el

(autoload 'capf-autosuggest-mode "capf-autosuggest" "\
Auto-suggest first completion at point with an overlay.

This is a minor mode.  If called interactively, toggle the
`Capf-Autosuggest mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `capf-autosuggest-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'capf-autosuggest-define-partial-accept-cmd "capf-autosuggest" "\
Define a command NAME.
It will call COMMAND interactively, allowing it to move point
into an auto-suggested overlay.  COMMAND must not modify buffer.
NAME must not be called if variable
`capf-autosuggest-active-mode' is inactive.  NAME is suitable for
binding in `capf-autosuggest-active-mode-map'.

(fn NAME COMMAND)" nil t)
(autoload 'capf-autosuggest-history-capf "capf-autosuggest" "\
Completion-at-point function for history.
Supports `comint-mode', `eshell-mode' and the minibuffer.  In
comint end eshell, it is applicable only if point is after the
last prompt.

This function is useful for inclusion in
`capf-autosuggest-capf-functions'.")
(register-definition-prefixes "capf-autosuggest" '("capf-autosuggest-"))

;;; End of scraped data

(provide 'capf-autosuggest-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; capf-autosuggest-autoloads.el ends here
