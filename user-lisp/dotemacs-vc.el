;;; dotemacs-vc.el  -*- lexical-binding: t; -*-

;; Copyright © 2024-2026  Zhengyi Fu <i@fuzy.me>

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


(eval-when-compile (require 'dotemacs-core))

;;;; vc

;; vc-hooks
(setopt vc-follow-symlinks t
        vc-use-incoming-outgoing-prefixes t
        vc-auto-revert-mode t)

(after-load! vc
  (setopt
   vc-svn-diff-switches '("-x" "-u -p")
   vc-find-revision-no-save t
   vc-deduce-backend-nonvc-modes t ;; Deduce VC backend for all buffers
   vc-dir-auto-hide-up-to-date t))

(declare-function vc-read-backend "vc.el")
(defun +vc/dir-here (&optional backend)
  (interactive
   (list
    (and current-prefix-arg
         (vc-read-backend "Prompt: "))))
  (vc-dir default-directory backend))

(defun +project/vc-diff ()
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (vc-backend (or (and (eq (car-safe project) 'vc)
                              (cadr project))
                         (vc-responsible-backend root "no-error")
                         (vc-read-backend "Backend: ")))
         (default-directory root))
    (vc-diff nil nil (list vc-backend (list root)))))
;;;; magit

(after-load! project
  (when (consp project-switch-commands)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))
(defvar magit-credential-cache-daemon-process)
(defun +magit--ccdp-no-query ()
  "Avoid query process status on exit."
  (when (bound-and-true-p magit-credential-cache-daemon-process)
    (set-process-query-on-exit-flag
     magit-credential-cache-daemon-process nil)))
(after-load! magit
  (transient-set-default-level 'magit:--gpg-sign 1)
  (transient-set-default-level 'magit:--signoff 1)
  (setq-default magit-tramp-pipe-stty-settings 'pty)
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)
  (advice-add #'magit-maybe-start-credential-cache-daemon :after '+magit--ccdp-no-query)
  (setopt magit-wip-mode-lighter "")
  (magit-wip-mode))

(after-load! magit-status
  (cl-pushnew "--show-signature" (get 'magit-status-mode 'magit-log-default-arguments)))

(after-load! magit-log
  (cl-pushnew "--show-signature" (get 'magit-log-mode 'magit-log-default-arguments)))

(declare-function magit-staged-files "magit-commit.el")
(defun +git-commit--log-edit-h ()
  (when (string-empty-p (buffer-substring-no-properties (point-min) (line-end-position 1)))
    (let ((params
           `((log-edit-listfun . ,#'magit-staged-files)
             (log-edit-diff-function . ,#'magit-diff-while-committing))))
      (dolist (crt params)
        (set (make-local-variable (car crt)) (cdr crt)))
      (run-hooks 'log-edit-hook)
      (save-buffer))))

(after-load! git-commit
  (setopt git-commit-major-mode #'log-edit-mode)
  (add-hook 'git-commit-post-finish-hook #'log-edit-hide-buf)
  (add-hook 'git-commit-setup-hook #'+git-commit--log-edit-h 90))

;;;; forge

(after-load! magit
  (require 'forge))

;;;; Add-Log

(defvar add-log-always-start-new-record)
(defvar add-log-buffer-file-name-function)
(declare-function add-log-find-changelog-buffer "log-edit.el")
(define-advice add-change-log-entry
    (:around (fun whoami changelog-file-name &rest rest) always-start-new-record)
  "Ensure a new change log entry is always started if file is unmodified.

This advice ensures that a new change log entry is always started if the
current change log file has not been modified. This behavior is useful
for automating the creation of change log entries, especially when
running commands in bulk or using version control systems that expect a
new entry for each change.

If the change log file is managed by version control and up-to-date, or
if no version control system is being used, this advice will set
`add-log-always-start-new-record' to t, thereby forcing the creation of
a new change log entry.

The main logic involves checking the modification status of the current
change log buffer and determining whether the change log file is under
version control. If the file is up-to-date or not under version control,
it sets the `add-log-always-start-new-record' variable to t to ensure a
new record is started."
  (interactive (list current-prefix-arg
                     (prompt-for-change-log-name)))
  (let* ((add-log-always-start-new-record add-log-always-start-new-record)
         (buf-file-name (funcall add-log-buffer-file-name-function))
         (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
         (changelog-file-name (expand-file-name (find-change-log
                                                 changelog-file-name
                                                 buffer-file)))
         (changelog-buf (add-log-find-changelog-buffer changelog-file-name)))
    (unless (buffer-modified-p changelog-buf)
      (if (vc-registered changelog-file-name)
          (when (vc-up-to-date-p changelog-file-name)
            (setq add-log-always-start-new-record t))
        (setq add-log-always-start-new-record t)))
    (apply fun whoami changelog-file-name rest)))

;;;; diff-hl

(autoload 'diff-hl-magit-post-refresh "diff-hl")
(add-hook 'tty-setup-hook #'diff-hl-margin-mode)

(add-hook 'dired-mode-hook #'diff-hl-dired-mode)

(after-load! magit-mode
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(defvar diff-hl-side)
(defvar diff-hl-margin-symbols-alist)

(after-load! diff-hl
  (diff-hl-flydiff-mode)
  (setopt diff-hl-margin-symbols-alist '((insert  . "增")
                                         (delete  . "刪")
                                         (change  . "改")
                                         (unknown . "疑")
                                         (ignored . "略")))

  (setopt diff-hl-update-async (fboundp 'make-thread))
  (add-hook 'diff-hl-margin-mode-hook #'diff-hl-margin-ensure-visible))

(define-advice diff-hl-margin-ensure-visible (:override () auto-width)
  "Ensure that diff-hl margin is wide enough to display all symbols.
Calculate the maximum width of all symbols in `diff-hl-margin-symbols-alist'
and set the appropriate margin width variable accordingly.
Then refresh all windows displaying the current buffer."
  (require 'map)
  (let ((width-var (intern (format "%S-margin-width" diff-hl-side))))
    (set width-var (apply #'max (map-values-apply #'string-width diff-hl-margin-symbols-alist))))
  (dolist (win (get-buffer-window-list))
    (set-window-buffer win (current-buffer))))

(after-load! (:or diff-hl vc magit)
  (global-diff-hl-mode)
  (keymap-set diff-hl-mode-map "C-c v" diff-hl-command-map))

;;;; consult-git-log-grep

(defun +consult-git-log-grep-show-commit (sha)
  "Displays the result of `git show SHA' in a new buffer."
  (let* ((default-directory (vc-git-root (or (buffer-file-name) default-directory))))
    (magit-show-commit sha)))

(after-load! consult-git-log-grep
  (setopt consult-git-log-grep-open-function #'+consult-git-log-grep-show-commit
          consult-git-log-grep-preview t))

;;;; quilt

(defun quilt-new-patch (patch-name)
  (interactive "sNew patch name:")
  (unless (string-suffix-p "patch" patch-name)
    (setq patch-name (concat patch-name ".patch")))
  (shell-command (concat "quilt new " (shell-quote-argument patch-name))))

(defun quilt-add-visited-file ()
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (let ((basename (file-name-nondirectory file)))
        (shell-command (concat "quilt add " (shell-quote-argument basename))))
    (user-error "Buffer not visiting a file")))

(defun quilt-list-files ()
  (interactive)
  (shell-command "quilt files"))

(defun quilt-refresh ()
  (interactive)
  (shell-command "quilt refresh"))

(defun quilt-pop (arg)
  (interactive "p")
  (while (> arg 0)
    (shell-command "quilt pop")
    (cl-decf arg)))

(defun quilt-push (arg)
  (interactive "p")
  (while (> arg 0)
    (shell-command "quilt push")
    (cl-decf arg)))

(defun quilt-list-applied-patches ()
  (interactive)
  (shell-command "quilt applied"))


(provide 'dotemacs-vc)
;;; dotemacs-vc.el ends here
