;;; exwm-settings.el --- EXWM settings -*- lexical-binding: t -*-
;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

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
;;; Code:

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
(exwm-xim-mode)
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

;;;; firefox

(straight-use-package
 '(exwm-firefox :host codeberg :repo "emacs-weirdware/exwm-firefox"))
(require 'exwm-firefox)
(exwm-firefox-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
