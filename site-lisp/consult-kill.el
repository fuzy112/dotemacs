;;; consult-kill.el --- Interactively kill a process -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: tools

;;; Commentary:

;;; TODO:

;; Add `embark' integration.

;;; Code:

(require 'consult)

;;;###autoload
(defun consult-kill (&optional initial)
  "Search and kill a process given INITIAL input."
  (interactive)
  (let ((pid
	 (consult--read
	  (cdr (process-lines "ps" "-ef"))
	  :prompt "Kill process: "
	  :initial initial
	  :require-match t
	  :category 'consult-kill
	  :lookup (lambda (cand _ _ _)
		    (string-to-number (nth 1 (split-string cand " " t)))))))
    (shell-command (format "( kill %d && sleep 4 && kill -KILL %1$d || true ) &" pid))))

(provide 'consult-kill)
;;; consult-kill.el ends here
