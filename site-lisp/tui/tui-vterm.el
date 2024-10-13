;;; tui-vterm.el --- VTerm support for TUI -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords:

;;; Commentary:

;; To use TUI with VTerm, eval the following code:

;;  (setq tui-terminal-function #'tui-vterm-exec)

;;; Code:

(require 'vterm nil t)

(define-derived-mode tui-vterm-mode vterm-mode "VTerm[TUI]")

;;;###autoload
(defun tui-vterm-exec (name command callback)
  (let* ((dir default-directory)
	 (vterm-shell (format "sh -c %s" (shell-quote-argument command)))
	 (vterm-buffer-name (concat "*" name "*"))
	 (vterm-kill-buffer-on-exit nil)
	 (vterm-mode-hook nil))
    (with-current-buffer (get-buffer-create vterm-buffer-name)
      (setq default-directory dir)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (pop-to-buffer (current-buffer)
		     '((display-buffer-pop-up-frame)
		       (pop-up-frame-parameters
			(auto-hide-function . delete-frame))))
      (tui-vterm-mode)
      (set-process-sentinel
       (get-buffer-process (current-buffer))
       (lambda (p _m)
	 (unless (process-live-p p)
	   (when (zerop (process-exit-status p))
	     (quit-windows-on (process-buffer p) nil 0)
	     (select-frame-set-input-focus (selected-frame)))
	   (with-current-buffer (process-buffer p)
	     (kill-local-variable 'change-major-mode-hook)
	     (let ((inhibit-read-only t))
	       (vterm--redraw vterm--term)))
	   (funcall callback p)))))))

(provide 'tui-vterm)
;;; tui-vterm.el ends here
