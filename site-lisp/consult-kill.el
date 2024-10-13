;;; consult-kill.el --- Interactively kill a process -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: tools

;;; Commentary:

;;; TODO:

;; Add `embark' integration.

;;; Code:

(require 'consult)
(eval-when-compile
  (require 'subr-x))

;;;###autoload
(defun consult-kill (&optional initial)
  "Search and kill a process given INITIAL input."
  (interactive)
  (let ((pid
	 (consult--read
	  (thread-first
	    (consult--async-sink)
	    (consult--dynamic-compute
	     (lambda (&optional _arg)
	       (cdr (process-lines "ps" "-ef"))))
	    (consult--async-throttle))
	  :prompt "Kill process: "
	  :initial initial
	  :require-match t
	  :category 'consult-kill
	  :lookup (lambda (cand _ _ _)
		    (string-to-number (nth 1 (split-string cand " " t)))))))
    (shell-command (format "kill %d" pid))))

(provide 'consult-kill)
;;; consult-kill.el ends here
