;;; EXWM settings -*- lexical-binding: t; -*-

(straight-use-package
 '(xelb :host github :repo "emacs-exwm/xelb"))
(straight-use-package
 '(exwm :host github :repo "emacs-exwm/exwm"))
(require 'exwm)

;; set the initial workspace number
(setq exwm-workspace-number 4)
;; make class name the buffer name
(add-hook 'exwm-update-class-hook
	  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

;; global key bindings
(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)
	([?\s-w] . exwm-workspace-switch)
	([?\s-&] . (lambda (cmd)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command cmd nil cmd)))
	([?\s-0] . +exwm-workspace-switch-create/dwim)
	([?\s-1] . +exwm-workspace-switch-create/dwim)
	([?\s-2] . +exwm-workspace-switch-create/dwim)
	([?\s-3] . +exwm-workspace-switch-create/dwim)
	([?\s-4] . +exwm-workspace-switch-create/dwim)
	([?\s-5] . +exwm-workspace-switch-create/dwim)
	([?\s-6] . +exwm-workspace-switch-create/dwim)
	([?\s-7] . +exwm-workspace-switch-create/dwim)
	([?\s-8] . +exwm-workspace-switch-create/dwim)
	([?\s-9] . +exwm-workspace-switch-create/dwim)))

(defun +exwm-workspace-switch-create/dwim ()
  (interactive)
  (let* ((keys (this-command-keys-vector))
	 (last-key (aref keys (1- (length keys))))
	 (digit (- (logand 127 last-key) ?0)))
    (exwm-workspace-switch-create digit)))

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
	([?\C-f] . [right])
	([?\C-p] . [up])
	([?\C-n] . [down])
	([?\C-a] . [home])
	([?\C-e] . [end])
	([?\M-v] . [prior])
	([?\C-v] . [next])
	([?\C-d] . [delete])
	([?\C-k] . [S-end delete])
	;; CUA
	([?\M-w] . [?\C-c])
	([?\C-y] . [?\C-v])
	([?\C-w] . [?\C-x])))

(exwm-systemtray-mode)
(exwm-enable)
;(exwm-xim-mode)
(push ?\C-\\ exwm-input-prefix-keys)

;;;; GPG pinentry

(setenv "GPG_AGENT_INFO" nil)
(setq auth-source-debug t)

(straight-use-package 'pinentry)
(require 'epa-file)
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)
(pinentry-start)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
