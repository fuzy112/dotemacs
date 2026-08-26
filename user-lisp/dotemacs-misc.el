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
  (setq eat-enable-auto-line-mode t)
  (setq eat-kill-buffer-on-exit t)
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
  (setq! eshell-visual-commands nil)
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
(defvar eat--line-input-ring)
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

;;;; shell-command+

(after-load! shell-command+
  (alist-setq! shell-command+-substitute-alist
    "ugrep" #'shell-command+-cmd-grep
    "ug" #'shell-command+-cmd-grep
    "rg" #'shell-command+-cmd-grep))

;;;; xterm

(setq! xterm-set-window-title t
       xterm-update-cursor t)

(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(after-load! xt-mouse
  (xterm-mouse-mode))
(if (eq (framep-on-display) t)
    (require 'xt-mouse))

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

(setq! bookmark-save-flag 1
       bookmark-watch-bookmark-file 'silent
       bookmark-version-control t
       bookmark-fringe-mark nil)

(after-load! bookmark
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

(setq! proced-auto-update-flag 'visible)
(setq! proced-auto-update-interval 1)

;;;; SHR

(setq! shr-use-colors nil)
(setq! shr-use-fonts nil)

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

(defun eww+redirect-to-old-reddit (url)
  (replace-regexp-in-string
   "\\`https?://\\(www\\.\\)?reddit\\.com\\(\\'\\|/\\)"
   "https://old.reddit.com/"
   url))

(with-eval-after-load 'eww
  (add-hook 'eww-url-transformers #'eww+redirect-to-old-reddit))

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
                  (call-interactively #'elfeed-show-visit))))
  (setopt elfeed-feeds (with-temp-buffer
                         (insert-file-contents (dotemacs-state-file "elfeed/feeds.eld"))
                         (goto-char (point-min))
                         (read (current-buffer))))
  (setopt elfeed-entry-point 'elfeed-tree))

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

;;;; dired

(setq! dired-listing-switches "-lahFbs"
       dired-hide-details-hide-absolute-location t
       dired-do-revert-buffer t
       dired-dwim-target t
       dired-auto-revert-buffer t
       dired-mouse-drag-files t
       dired-recursive-copies 'always
       dired-recursive-deletes 'always
       shell-command-prompt-show-cwd t)

;;;; zone

(setq! zone-all-frames t
       zone-all-windows-in-frame t)

;;;; send-to

(define-completion-category 'send-to-tailscale-target ()
  "Completion category for `send-to/tailscale-send-items' targets."
  :style '(substring))

(declare-function send-to--convert-item-to-filename "send-to")

(defun send-to/tailscale-supported-p ()
  (executable-find "tailscale"))
(defun send-to/tailscale-send-items (items)
  (let* ((files (mapcar #'send-to--convert-item-to-filename items))
         (candidates (process-lines "tailscale" "file" "cp" "--targets"))
         (table (completion-table-with-metadata candidates '((category . send-to-tailscale-target))))
         (target (car (split-string (completing-read "Target: " table nil t))))
         (command (concat "tailscale file cp " (combine-and-quote-strings files " ") " " target ":")))
    (when (string-empty-p target)
      (user-error "No target specified"))
    (compile command)))

(defun send-to/bluetooth-supported-p ()
  (executable-find "blueman-sendto"))
(defun send-to/bluetooth-send-items (items)
  (let* ((files (mapcar #'send-to--convert-item-to-filename items))
         (devices (process-lines "bluetoothctl" "devices"))
         (table (completion-table-with-metadata devices '((category . send-to-bluetooth-target))))
         (target (cadr (split-string (completing-read "Target: " table nil t))))
         (command (concat "blueman-sendto -d " target " " (combine-and-quote-strings files " "))))
    (compile command)))
(after-load! send-to
  (add-to-list 'send-to-handlers
               '((:supported . send-to/bluetooth-supported-p)
                 (:collect . send-to--collect-items)
                 (:send . send-to/bluetooth-send-items)))
  (add-to-list 'send-to-handlers
               '((:supported . send-to/tailscale-supported-p)
                 (:collect . send-to--collect-items)
                 (:send . send-to/tailscale-send-items))))

;;;; uptime

;; Set up a timer to display emacs uptime every 30 min.

(defun uptime-notify ()
  (message "Emacs has been running for %s" (emacs-uptime)))

(defvar uptime-notification-timer
  (run-with-timer 1800 1800 #'uptime-notify))

;;;; ffap-menu

(defvar ffap-menu-alist)

(declare-function shr-url-at-point "shr")

(define-advice ffap-menu-rescan (:around (&rest args) shr)
  (let ((inhibit-message t))
    (apply args)
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward 'shr-tab-stop nil nil t)))
        (goto-char (prop-match-beginning match))
        (when-let* ((item (get-text-property (point) 'shr-url)))
          (push (cons item (point)) ffap-menu-alist))))
    ;; deduplicate
    (setq ffap-menu-alist
          (sort ffap-menu-alist
                (lambda (a b) (string-lessp (car a) (car b)))))
    (let ((ptr ffap-menu-alist))
      (while (cdr ptr)
        (if (equal (car (car ptr)) (car (car (cdr ptr))))
            (setcdr ptr (cdr (cdr ptr))))
        (setq ptr  (cdr ptr))))
    ;; sort
    (setq ffap-menu-alist
          (sort ffap-menu-alist (lambda (a b) (< (cdr a) (cdr b)))))))

(declare-function minibuffer-selected-candidate "dotemacs-completion")

(defvar-local ffap-menu--alist nil)
(defvar-local ffap-menu--preview-window nil)
(defvar-local ffap-menu--preview-buffer nil)

(defun ffap-menu--post-command-preview ()
  (when-let* (((buffer-live-p ffap-menu--preview-buffer))
              (cand (minibuffer-selected-candidate))
              (pos (alist-get cand ffap-menu--alist nil nil #'string=)))
    (if (window-live-p ffap-menu--preview-window)
        (set-window-buffer ffap-menu--preview-window
                           ffap-menu--preview-buffer)
      (setq-local ffap-menu--preview-window
                  (display-buffer ffap-menu--preview-buffer)))
    (with-selected-window ffap-menu--preview-window
      (unless (= (point) pos)
        (goto-char pos)
        (recenter)
        (pulse-momentary-highlight-one-line)))))

(when (fboundp 'timeout-debounce)
  (timeout-debounce 'ffap-menu--post-command-preview))

(define-advice ffap-menu-ask (:around (&rest args) preview)
  (require 'dotemacs-completion)
  (let ((alist ffap-menu-alist)
        (preview-window (selected-window))
        (preview-buffer (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local ffap-menu--alist alist
                      ffap-menu--preview-window preview-window
                      ffap-menu--preview-buffer preview-buffer)
          (add-hook 'post-command-hook #'ffap-menu--post-command-preview nil t))
      (save-excursion
        (apply args)))))

(declare-function url-type "url-parse")

(defun ffap-menu-to-url (_type target)
  (and-let* ((url (with-minibuffer-selected-window
                    (save-excursion
                      (goto-char (cdr (assoc target ffap-menu-alist)))
                      (or (ffap-url-at-point)
                          (shr-url-at-point nil)))))
             (urlobj (url-generic-parse-url url)))
    (pcase (url-type urlobj)
      ((or 'file 'nil)
       (cons 'file url))
      (_
       (cons 'url url)))))

(after-load! embark
  (alist-setq! embark-transformer-alist
    ffap-menu #'ffap-menu-to-url))

;;;; Disable GC before running other kill-emacs-hook functions

(defun kill-emacs/disable-gc ()
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0))

(add-hook 'kill-emacs-hook #'kill-emacs/disable-gc -20)


(put 'help-fns-edit-variable 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'narrow-to-region 'disabled nil)


(provide 'dotemacs-misc)
;;; dotemacs-misc.el ends here
