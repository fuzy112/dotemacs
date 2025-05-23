;;; tui.el --- Run TUI tools in terminal -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu
;; Version:  0.1.1
;; Keywords: tools

;;; Commentary:
;;; Code:

(require 'fuzzy-finder)

(defgroup tui ()
  "TUI tools support."
  :group 'tools)

(autoload 'tui-eat-exec "tui-eat")
(autoload 'tui-vterm-exec "tui-vterm")
(autoload 'tui-term-exec "tui-term")

(defcustom tui-external-terminal-program "x-terminal-emulator"
  "The default external terminal terminal emulator."
  :type 'string)

(defun tui-external-terminal-exec (name command callback)
  (let* ((dir default-directory)
	 (buffer (with-current-buffer
		     (get-buffer-create (concat "*" name "*"))
		   (setq default-directory dir)
		   (erase-buffer)
		   (current-buffer)))
	 (ofile (make-temp-file "tui-output."))
	 (proc (start-file-process
		name buffer tui-external-terminal-program
		"-e" shell-file-name "-c"
		(format "( %s ) >%s" command ofile))))
    (set-process-sentinel
     proc
     (lambda (p _m)
       (unwind-protect
	   (unless (process-live-p p)
	     (with-current-buffer buffer
	       (erase-buffer)
	       (insert-file-contents ofile))
	     (funcall callback p))
	 (unless (process-live-p p)
	   (kill-buffer buffer)
	   (delete-file ofile)))))))

(defcustom tui-terminal-function (cond ((fboundp 'eat) #'tui-eat-exec)
				       ((fboundp 'vterm) #'tui-vterm-exec)
				       ((fboundp 'term) #'tui-term-exec)
				       (t #'tui-external-terminal-exec))
  "Function called to start terminals.

See `tui-run' for the function signature."
  :type '(choice (const :tag "Eat" tui-eat-exec)
		 (const :tag "VTerm" tui-vterm-exec)
		 (const :tag "Term" tui-term-exec)
		 (const :tag "Default terminal emulator" tui-external-terminal-exec)
		 (function :tag "Custom")))

;;;###autoload
(defun tui-run (name command &optional callback)
  "Execute COMMAND in a terminal.

NAME is the name of the process.  It is also used to determine
the buffer name.

CALLBACK is called when the process exits.  It accepts a
single argument, the process."
  (interactive (list "tui-run" (read-shell-command "Command: ") #'ignore))
  (funcall tui-terminal-function name command (or callback #'ignore)))

(defcustom tui-project-function #'tui--default-project-function
  "A function that returns the root directory of the current project."
  :type 'function)

(declare-function project-root "project")
(declare-function project-current "project")

(defun tui--default-project-function (may-prompt)
  (and-let* ((p (project-current may-prompt)))
    (project-root p)))

(defun tui--project-root (&optional may-prompt)
  (and-let* ((root (and (functionp tui-project-function)
			(funcall tui-project-function may-prompt))))
    (expand-file-name root)))

(defconst tui--grep-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):[[:space:]]*\\(.*\\)$")

(defun tui--grep-callback (proc)
  "Pop to the file location in the buffer of PROC."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (while (re-search-forward tui--grep-regexp nil t)
      (let* ((file (match-string 1))
	     (line (string-to-number (match-string 2)))
	     (col (string-to-number (match-string 3))))
	(save-current-buffer
	  (unless (file-exists-p file)
	    (error "tui--grep-callback: file not exist: %s" file))
	  (find-file file)
	  (goto-char (point-min))
	  (forward-line (1- line))
	  (forward-char (1- col)))))))

(defconst tui--grep-preview "bat --force-colorization --highlight-line={2} -- {1}")

(defconst tui--dir
  (file-name-parent-directory (or load-file-name buffer-file-name)))

(defun tui--exec-dir (arg)
  (if arg
      (read-directory-name "Dir: " nil nil t)
    (or (tui--project-root) default-directory)))

;;;###autoload
(defun tui-rg (arg)
  "Run `rg' in a terminal and jump to the selected line.
When called interactively, the symbol at point is used as the initial query."
  (interactive "P")
  (let ((default-directory (tui--exec-dir arg)))
    (tui-run
     "tui-rg"
     (fuzzy-finder-command
      :interactive t
      :ansi t
      :bind '("ctrl-k:kill-line")
      :cmd-prompt "ripgrep> "
      :cmd "rg --column --color=always --line-number --smart-case -- {}"
      :delimiter ":"
      :preview tui--grep-preview
      :preview-window "+{2}-/3")
     #'tui--grep-callback)))

;;;###autoload
(defun tui-ugrep (arg)
  "Run `ugrep' in a terminal and jump to the selected line.
When called interactively, the symbol at point is used as the initial query."
  (interactive "P")
  (let ((default-directory (tui--exec-dir arg)))
    (tui-run
     "tui-ugrep"
     (fuzzy-finder-command
      :interactive t
      :ansi t
      :bind '("ctrl-k:kill-line")
      :cmd-prompt "ugrep> "
      :cmd "ugrep --with-filename --color=always --line-number --column-number --smart-case --ignore-binary -- {}"
      :delimiter ":"
      :preview tui--grep-preview
      :preview-window "+{2}-/3")
     #'tui--grep-callback)))

;;;###autoload
(defun tui-line (arg)
  (interactive "P")
  (let ((file
	 (file-relative-name
	  (if (or (numberp arg) (not (buffer-file-name)))
	      (read-file-name "Query file: " default-directory nil t)
	    (buffer-file-name)))))
    (save-some-buffers)
    (tui-run
     "tui-line"
     (fuzzy-finder-command
      :interactive nil
      :ansi t
      :bind '("ctrl-k:kill-line")
      :prompt "line> "
      :cmd (format "rg --column --color=always --with-filename --line-number --smart-case -- {} %s" (shell-quote-argument file))
      :delimiter ":"
      :preview tui--grep-preview
      :preview-window "up:60%:+{2}-/3")
     #'tui--grep-callback)))

(defun tui--file-callback (proc)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (while (re-search-forward "^.+$" nil t)
      (save-current-buffer
	(let ((file (match-string 0)))
	  (when (file-exists-p file)
	    (find-file file)))))))


;;;###autoload
(defun tui-yazi (arg)
  (interactive "P")
  (let ((default-directory (tui--exec-dir arg)))
    (tui-run "tui-yazi"
	     "tmpfile=$(mktemp) && yazi --chooser-file=$tmpfile && cat $tmpfile ; rm -f $tmpfile"
	     #'tui--file-callback)))

(defconst tui--file-preview
  "[ -d {} ] && ls -lBh --color=always {} || bat --force-colorization -- {}")

;;;###autoload
(defun tui-find (arg)
  (interactive "P")
  (let ((default-directory  (tui--exec-dir arg)))
    (tui-run
     "tui-find"
     (fuzzy-finder-command
      :ansi t
      :bind '("ctrl-k:kill-line")
      :cmd "fd . || fd-find . || find ."
      :preview tui--file-preview
      :preview-window "up:60%:+{2}/3")
     #'tui--file-callback)))

;;;###autoload
(defun tui-locate ()
  (interactive)
  (tui-run
   "tui-locate"
   (fuzzy-finder-command
    :ansi t :interactive t
    :bind '("ctrl-k:kill-line")
    :cmd-prompt "locate> "
    :cmd "locate {}"
    :preview tui--file-preview)
   #'tui--file-callback))

;;;###autoload
(defun tui-lazygit ()
  (interactive)
  (tui-run "tui-lazygit" "lazygit"))

;;;###autoload
(defun tui-btop ()
  (interactive)
  (tui-run "tui-btop" "btop"))

;;;###autoload
(defun tui-htop ()
  (interactive)
  (tui-run "tui-htop" "htop"))

;;;###autoload
(defun tui-nmtui ()
  (interactive)
  (tui-run "tui-nmcli" "nmtui"))

;;;###autoload
(defun tui-mc ()
  (interactive)
  (tui-run "tui-mc" "mc"))

(defun tui--kill-callback (proc)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\([[:digit:]]+\\)" nil t)
      (let ((pid (match-string 1)))
	(shell-command (format "kill -TERM %s && sleep 4 && kill -KILL %1$s || true" pid))))))

;;;###autoload
(defun tui-kill ()
  (interactive)
  (tui-run
   "tui-kill"
   (fuzzy-finder-command
    :ansi t
    :bind '("ctrl-k:kill-line")
    :cmd "ps -ax"
    :header-lines 1
    :layout "reverse"
    :preview "ps -p {2} -F"
    :preview-window "up:5")
   #'tui--kill-callback))

;;;###autoload
(defun tui-lnav (file)
  (interactive "fLog file: ")
  (tui-run "tui-lnav"
	   (format "lnav %s" (shell-quote-argument (expand-file-name file)))))


(declare-function recentf-save-list "recentf")
(defvar recentf-save-file)

;;;###autoload
(defun tui-recentf ()
  (interactive)
  (require 'recentf)
  (when recentf-mode
    (recentf-save-list))
  (tui-run "tui-recentf"
	   (fuzzy-finder-command
	    :ansi t
	    :bind '("ctrl-k:kill-line")
	    :cmd (format "%s -Q --batch --eval %s"
			 (expand-file-name invocation-name invocation-directory)
			 (shell-quote-argument
			  (prin1-to-string
			   `(progn
			      (load ,recentf-save-file)
			      (dolist (file recentf-list)
				(princ (expand-file-name file))
				(princ "\n"))))))
	    :preview tui--file-preview
	    :preview-window "up:60%:+{2}/3")
	   #'tui--file-callback))

;;;###autoload
(defun tui-git-ls-files ()
  (interactive)
  (tui-run "tui-git-ls-files"
	   (fuzzy-finder-command
	    :ansi t
	    :bind '("ctrl-k:kill-line")
	    :cmd "git ls-files"
	    :preview tui--file-preview
	    :preview-window "up:60%:+{2}/3")
	   #'tui--file-callback))

(provide 'tui)
;;; tui.el ends here
