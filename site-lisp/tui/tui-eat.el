;;; tui-eat.el --- Eat support for TUI -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu
;; Version: 0.1.0
;; Keywords:

;;; Commentary:

;; To use Eat with TUI, eval the following code:

;;   (setq tui-terminal-function #'tui-eat-exec)

;; If you want to display the buffer in a child frame, add the
;; following code to your init.el:
;;
;;   (setf (alist-get '(derived-mode . tui-eat-mode) display-buffer-alist nil nil #'equal)
;;         '((display-buffer-in-child-frame)
;;           (child-frame-parameters (undecorated . t)
;;                                   (tab-bar-lines . 0)
;;                                   (tool-bar-lines . 0)
;;                                   (menu-bar-lines . 0)
;;                                   (vertical-scroll-bars . nil)
;;                                   (width . 1.0)
;;                                   (height . 1.0)
;;                                   (left . 0.0)
;;                                   (top . 0.0)
;;                                   (keep-ratio . t))))

;;; Code:

(require 'eat)

(defgroup tui-eat ()
  "Eat support for TUI."
  :group 'tui)

;; Define a derived mode of eat-mode to make it easy
;; to customize `display-buffer-alist'.
(define-derived-mode tui-eat-mode eat-mode "Eat[TUI]"
  (setq truncate-lines t))

;;;###autoload
(defun tui-eat-exec (name command callback)
  "Run COMMAND in an Eat buffer and display that buffer in a new frame.

The buffer name is made by surrounding NAME with `*'s.

The buffer is killed if the process exits normally.
CALLBACK is called after the process exits.

See `ee-start-terminal-function' and
`ee-start-process-shell-command-in-terminal'."
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create (concat "*" name "*"))
      (setq default-directory dir)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (when-let* ((p (get-buffer-process (current-buffer))))
	(delete-process p))
      (unless (eq major-mode #'tui-eat-mode)
	(tui-eat-mode))
      (pop-to-buffer (current-buffer)
		     '((display-buffer-pop-up-frame)
		       (pop-up-frame-parameters
			(auto-hide-function . delete-frame))))
      (kill-local-variable 'eat-exit-hook)
      (add-hook 'eat-exit-hook
		(lambda (p)
		  (with-current-buffer (process-buffer p)
		    (goto-char (point-max))
		    (forward-line -1)
		    (delete-region (line-beginning-position) (point-max))
		    (when (zerop (process-exit-status p))
		      (quit-windows-on (current-buffer) nil 0)))
		  (funcall callback p))
		nil t)
      (let (eat-exec-hook)
	(eat-exec (current-buffer) name "sh" nil (list "-c" command)))
      (eat-char-mode))))

(provide 'tui-eat)
;;; tui-eat.el ends here
