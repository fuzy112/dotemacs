;;; consult-kill.el --- Interactively kill a process -*- lexical-binding: t -*-
;; Copyright © 2024, 2025, 2026  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: tools

;;; Commentary:

;;; TODO:

;; Add `embark' integration.

;;; Code:

(require 'consult)

;;;###autoload
(defun consult-kill (&optional initial signal)
  "Read a process with given INITIAL input and send SIGNAL to it."
  (interactive
   (list nil (if current-prefix-arg
		 (completing-read "Signal: "
				  (signal-names)
				  nil t)
	       "INT")))
  (let ((pid
	 (consult--read
	  (cdr (process-lines "ps" "-ef"))
	  :prompt "Kill process: "
	  :initial initial
	  :require-match t
	  :category 'consult-kill
	  :lookup (lambda (cand _ _ _)
		    (string-to-number (nth 1 (split-string cand " " t)))))))
    (shell-command (format "( kill -%s %d" signal pid))))

(provide 'consult-kill)
;;; consult-kill.el ends here
