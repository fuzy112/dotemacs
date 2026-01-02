;;; tui-term.el --- Ansi-term support for TUI -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: tools

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

;; To use Term with TUI, eval the following code:

;;   (setq tui-terminal-function #'tui-term-exec)

;;; Code:


(require 'term)

(define-derived-mode tui-term-mode term-mode "Term[TUI]")

;;;###autoload
(defun tui-term-exec (name command callback)
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create (concat "*" name "*"))
      (setq default-directory dir)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (delay-mode-hooks
	(unless (eq major-mode 'tui-term-mode)
	  (tui-term-mode))
	(pop-to-buffer (current-buffer)
		       '((display-buffer-pop-up-frame)
			 (pop-up-frame-parameters
			  (auto-hide-function . delete-frame))))
	(let (term-exec-hook)
	  (term-exec (current-buffer) name "sh" nil
		     (list "-c" command)))
	(term-char-mode)
	(set-process-sentinel
	 (get-buffer-process (current-buffer))
	 (lambda (proc msg)
	   (unless (process-live-p proc)
	     (when (zerop (process-exit-status proc))
	       (quit-windows-on (process-buffer proc) nil 0)
	       (select-frame-set-input-focus (selected-frame)))
	     (when callback
	       (funcall callback proc)))
	   (term-sentinel proc msg)))))))

(provide 'tui-term)
;;; tui-term.el ends here
