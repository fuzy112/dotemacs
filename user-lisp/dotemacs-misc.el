;;; dotemacs-misc.el  -*- lexical-binding: t; -*-

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


;;;; pdf

(autoload 'pdf-view-mode "pdf-tools" nil t)

(alist-setq! auto-mode-alist "\\.pdf\\'" #'pdf-view-mode)
(alist-setq! magic-mode-alist "%PDF" #'pdf-view-mode)

;;;; eat

(after-load! eat
  (setopt eat-enable-auto-line-mode t)
  (setopt eat-kill-buffer-on-exit t)
  (setopt eat-semi-char-non-bound-keys
          (seq-union '([?\e ?o])
                     eat-semi-char-non-bound-keys))
  (defvar eat--line-mode)
  (add-hook 'eat--line-mode-hook
            (lambda ()
              (corfu-mode (if eat--line-mode +1 -1))))
  (keymap-set eat-line-mode-map "M-h" #'cape-eat-line-history)
  (keymap-set eat-line-mode-map "M-r" #'consult-history)
  )

(unless (memq system-type '(ms-dos windows-nt))
  (setopt eshell-visual-commands nil)
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(defvar eat-terminal)

(defvar eat-term-terminfo-directory)
(defvar eat-term-shell-integration-directory)
(defun +eat-install-helpers ()
  (interactive)
  (unless (file-remote-p default-directory)
    (user-error "Not in a remote directory"))
  (copy-directory eat-term-terminfo-directory
                  (concat (file-remote-p default-directory) "~/.terminfo")
                  nil t t)
  (copy-directory eat-term-shell-integration-directory
                  (concat (file-remote-p default-directory) "~/.cache/eat-shell-integration")
                  nil t t))

(declare-function cape-interactive "ext:cape")
(defvar cape--history-properties)
(defun cape-eat-line-history (&optional interactive)
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-eat-line-history)
    (when-let* ((history (ring-elements eat--line-input-ring))
                (bol (line-beginning-position)))
      `(,bol ,(point) ,history ,@cape--history-properties))))

;;;; with-editor

(setopt shell-command-with-editor-mode t)

(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'term-exec-hook #'with-editor-export-editor)
(add-hook 'vterm-mode-hook #'with-editor-export-editor)

;;;; shell-command

;; Don't display async shell command buffer until there is output.
(setq async-shell-command-display-buffer nil)

;;;; xterm

(after-load! term/xterm
  (setopt xterm-set-window-title t
          xterm-update-cursor t))

(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(after-load! xt-mouse
  (xterm-mouse-mode))
(if (eq (framep-on-display) t)
    (require 'xt-mouse))

;;;; clipetty

(add-hook 'tty-setup-hook #'clipetty-mode)
(after-load! clipetty
  (global-clipetty-mode))
(if (eq (framep-on-display) t) (require 'clipetty))

;;;; Characters

(setopt cjk-ambiguous-chars-are-wide nil)

;;;; emacs-server

;; Workaround windows encoding issue
(defun +server--process-filter-coding-system (&rest args)
  (let ((file-name-coding-system locale-coding-system))
    (apply args)))

(after-load! server
  ;; If the operating system is either Windows (windows-nt) or DOS (ms-dos),
  ;; add an advice around the server process filter function to correctly handle coding systems.
  (when (memq system-type '(windows-nt ms-dos))
    (advice-add #'server-process-filter :around '+server--process-filter-coding-system)))

(defun +import-env-var-for-display ()
  (let ((display-name (or (frame-parameter nil 'display) x-display-name)))
    (pcase display-name
      ('nil nil)
      ((pred (string-match-p "\\`wayland-"))
       (setenv "WAYLAND_DISPLAY" display-name))
      ((pred (string-match-p "\\`:"))
       (setenv "DISPLAY" display-name)))))

(add-hook 'server-after-make-frame-hook #'+import-env-var-for-display)

(defun +niri-xdg-open (url &optional _ignored)
  (call-process "niri" nil 0 nil
                "msg" "action" "spawn" "--" "xdg-open" url))

(function-put '+niri-xdg-open 'browse-url-browser-kind 'external)

(when (getenv "NIRI_SOCKET")
  (setopt browse-url-browser-function #'+niri-xdg-open))

;;;; bookmark

(defvar pp-default-function)
(defun +bookmark--pp-28 (&rest args)
  (let ((pp-default-function 'pp-28))
    (apply args)))

(after-load! bookmark
  (advice-add #'bookmark-write-file :around '+bookmark--pp-28)

  (setopt bookmark-save-flag 1
          bookmark-watch-bookmark-file 'silent
          bookmark-version-control t
          bookmark-fringe-mark nil)

  (require 'bookmark-extras))

;;;; proced

(define-advice proced-format-args (:override (args) nix)
  (if-let* ((splitted (split-string args))
            (exe (car splitted))
            ((string-prefix-p "/nix/" exe)))
      (string-join
       (cons (file-name-nondirectory exe)
             (cdr splitted))
       " ")
    args))

;;;; SHR

(after-load! shr
  (setopt shr-use-colors nil)
  (setopt shr-use-fonts nil))

;;;; EWW

(declare-function eww-current-url "eww.el")

(defun eww+miniflux-trim ()
  (when (string-match-p "^https://miniflux\\." (eww-current-url))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when-let* ((match (text-property-search-forward 'shr-target-id "page-header-title" 'member)))
          (delete-region (point-min) (prop-match-beginning match)))))))

(add-hook 'eww-after-render-hook 'eww+miniflux-trim)

;;;; bangs

(after-load! bangs
  (setopt bangs-user-bangs
          '(("Mailing lists mirrored at yhetil.org" "yhetil"
             "https://yhetil.org/$1/?q=$2"
             :regex "(\\S+)\\s+(.*)" :fmt (url_encode_placeholder))
            ("Boss 直聘" "zhipin"
             "https://www.zhipin.com/web/geek/jobs?query={{{s}}}"
             :triggers ("boss"))
            ("知乎直达" "zhida" "https://zhida.zhihu.com/search?q={{{s}}}")
            ("Emacs China" "emacs-china"
             "https://emacs-china.org/search?q={{{s}}}"
             :triggers ("emacsc" "ec"))
            ("小红书" "xhs"
             "https://www.xiaohongshu.com/search_result_ai?keyword={{{s}}}"
             :triggers ("rednote" "redn"))))
  (setopt bangs-pretty-print-json t))

;;;; elfeed

(after-load! elfeed
  (keymap-set elfeed-show-mode-map "e"
              (lambda ()
                (interactive)
                (let ((browse-url-browser-function #'eww-browse-url))
                  (call-interactively #'elfeed-show-visit)))))

;;;; envrc

(after-init!
  (envrc-global-mode))

;;;; time report

(defun dotemacs-time-report ()
  (interactive)
  (let ((buffer (get-buffer-create "*dotemacs-time-report*")))
    (pop-to-buffer buffer)
    (erase-buffer)
    (dolist (item (sort dotemacs-time-alist :key (apply-partially #'nth 3) :reverse t ))
      (insert (truncate-string-to-width (prin1-to-string (car item)) 24))
      (insert (format "%s%f%s%f%s%f"
                      (propertize " " 'display '(space :align-to 25))
                      (float-time (nth 1 item))
                      (propertize " " 'display '(space :align-to 45))
                      (float-time (nth 2 item))
                      (propertize " " 'display '(space :align-to 65))
                      (float-time (nth 3 item)))
              "\n"))
    (goto-char (point-min))))

;;;; uptime

;; Set up a timer to display emacs uptime every 30 min.

(defun uptime-notify ()
  (message "Emacs has been running for %s" (emacs-uptime)))

(defvar uptime-notification-timer
  (run-with-timer 1800 1800 #'uptime-notify))


(put 'help-fns-edit-variable 'disabled nil)
(put 'list-timers 'disabled nil)


(provide 'dotemacs-misc)
;;; dotemacs-misc.el ends here
