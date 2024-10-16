;;; logos-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from logos.el

(autoload 'logos-forward-page-dwim "logos" "\
Move to next or COUNTth page forward.
If the buffer is narrowed, keep the effect while performing the
motion.  Always move point to the beginning of the narrowed
page.

(fn &optional COUNT)" t)
(autoload 'logos-backward-page-dwim "logos" "\
Move to previous or COUNTth page backward.
If the buffer is narrowed, keep the effect while performing the
motion.  Always move point to the beginning of the narrowed
page.

(fn &optional COUNT)" t)
(autoload 'logos-narrow-dwim "logos" "\
Do-what-I-mean narrowing.

If region is active, narrow the buffer to the region's
boundaries.

If pages are defined by virtue of `logos--page-p', narrow to
the current page boundaries.

If no region is active and no pages exist, narrow to the visible
portion of the window.

If narrowing is in effect, widen the view." t)
(autoload 'logos-focus-mode "logos" "\
Buffer-local mode for focused editing.

When enabled it sets the buffer-local value of these user
options: `logos-scroll-lock', `logos-variable-pitch',
`logos-hide-cursor', `logos-hide-mode-line',
`logos-hide-header-line', `logos-hide-buffer-boundaries',
`logos-buffer-read-only', `logos-olivetti', `logos-hide-fringe'.

This is a minor mode.  If called interactively, toggle the `Logos-Focus
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `logos-focus-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "logos" '("logos-"))

;;; End of scraped data

(provide 'logos-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; logos-autoloads.el ends here
