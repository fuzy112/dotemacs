;;; gpg-agent.el --- gpg-agent integration           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:

(defvar gpg-agent-buffer-name " *gpg-tty*")

;;;###autoload
(defun gpg-agent-terminal-start ()
  (interactive)
  
  ;; Unset GPG_TTY to prevent the pinentry from messing up emacs UI.
  (setenv "GPG_TTY")

  (when-let* ((proc (get-buffer-process " *gpg-tty*")))
    (set-process-buffer proc nil)
    (kill-process proc))

  ;; Allocate a PTY and use it as GPG_TTY.
  ;; NOTE: pinentry-curses is not supported.
  (let* ((buf (make-comint-in-buffer
	       "gpg-tty" " *gpg-tty*" "/bin/sh" nil
	       "-c" "GPG_TTY=$(tty) gpg-connect-agent updatestartuptty /bye && exec sleep infinity"))
	 (proc (get-buffer-process buf))
	 (tty (process-tty-name proc)))
    (with-current-buffer buf
      (unless (memq #'comint-watch-for-password-prompt (default-value 'comint-output-filter-functions))
	(add-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt nil t)))
    (set-process-query-on-exit-flag proc nil)
    (setenv "GPG_TTY" tty)))

(provide 'gpg-agent)
;;; gpg-agent.el ends here
