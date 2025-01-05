;;; exde.el --- Emacs X Desktop Environment -*- lexical-binding: t -*-
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
        ([?\s-9] . +exwm-workspace-switch-create/dwim)

        ;;; for MS-windows where s- cannot be used
        ([?\M-g ?w ?r] . exwm-reset)
        ([?\M-g ?w ?w] . exwm-workspace-switch)
        ([?\M-g ?w ?&] . (lambda (cmd)
                           (interactive (list (read-shell-command "$ ")))
                           (start-process-shell-command cmd nil cmd)))
        ([?\M-g ?w ?0] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?1] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?2] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?3] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?4] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?5] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?6] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?7] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?8] . +exwm-workspace-switch-create/dwim)
        ([?\M-g ?w ?9] . +exwm-workspace-switch-create/dwim)))

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
(cl-pushnew ?\C-\\ exwm-input-prefix-keys)


(cl-pushnew ?\M-o exwm-input-prefix-keys)
(cl-pushnew ?\M-s exwm-input-prefix-keys)

;;;; GPG pinentry

(setenv "GPG_AGENT_INFO" nil)
(setq auth-source-debug t)

(straight-use-package 'pinentry)
(setq epg-pinentry-mode 'loopback)
(pinentry-start)

(require 'org-crypt)
(org-crypt-use-before-save-magic)

;;;; rime

(declare-function rime-active-mode "rime.el" (&optional arg1))
(define-advice rime-input-method (:before (&rest _) exwm-xim)
  (when (bound-and-true-p exwm-xim-buffer-p)
    (setq-local rime--temporarily-ignore-predicates t)
    (rime-active-mode)))

;;;; firefox

(require 'exwm-firefox)
(exwm-firefox-mode)

;;;; display time

(display-time-mode)

;;;; screen capture

(defun exde-capture-and-display (command)
  "Run COMMAND to capture an image and display it in a buffer.
COMMAND is a list of strings specifying an executable and its
arguments.  The command should write the raw image to the standard
output."
  (let* ((buf (generate-new-buffer "*screen-capture*"))
         (process
          (make-process :name "scrot"
                        :buffer buf
                        :stderr (get-buffer-create " *scrot-stderr*")
                        :connection-type 'pipe
                        :command command
                        :sentinel (lambda (p _e)
                                    (unless (process-live-p p)
                                      (pop-to-buffer buf)
                                      (image-mode))))))))

(defun exde-capture-interactive (&optional delay)
  "Capture an image from an interactively selected window or rectangle.
Wait DELAY seconds before taking the shot."
  (interactive "p")
  (exde-capture-and-display
   (list "scrot" "-s" "-f" "-d" (number-to-string (or delay 0)) "-")))

(defun exde-capture-fullscreen (&optional delay)
  "Capture an image of the entire screen.
Wait DELAY seconds before taking the shot."
  (interactive "p")
  (exde-capture-and-display
   (list "scrot" "-d" (number-to-string (or delay 0)) "-")))

(defun +exwm--read-window-id (prompt)
  (let* ((collection (mapcar (lambda (pair)
                               (cons (buffer-name (cdr pair))
                                     (car pair)))
                             exwm--id-buffer-alist))
         (selected (completing-read prompt collection nil
                                    'require-match nil nil exwm--id))
         (id (alist-get selected collection nil nil #'equal)))
    (unless id (user-error "No window selected"))
    (number-to-string id)))

(defun exde-capture-xwindow (win &optional delay)
  "Capture an image from WIN.
WIN is a string representing an X11 window id.
Interactively, read a window id from the minibuffer.
Wait DELAY secondcs before taking the shot."
  (interactive
   (list
    (+exwm--read-window-id "Capture window: ")
    (prefix-numeric-value current-prefix-arg)))
  (exde-capture-and-display
   (list "scrot"
         "-d" (number-to-string (or delay 0))
         "-w" win
         "-")))

(defun exde-capture-frame (&optional frame delay)
  (interactive "ip")
  (exde-capture-xwindow
   (frame-parameter nil 'window-id)
   delay))

(defun exde-capture-rectangle (x y w h &optional delay)
  (exde-capture-and-display
   (list "scrot"
         "-a" (format "%d,%d,%d,%d" x y w h)
         "-d" (number-to-string (or delay 0))
         "-")))

(defun exde-capture-window (&optional win delay)
  (interactive "ip")
  (exde-capture-rectangle
   (window-pixel-left win)
   (window-pixel-top win)
   (window-pixel-width win)
   (window-pixel-height win)
   delay))


(provide 'exde)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; exde.el ends here
