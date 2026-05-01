;;; dotemacs-dired.el  -*- lexical-binding: t; -*-

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

;;;; dired

(add-hook 'dired-mode-hook #'dired-omit-mode)
(after-load! dired
  (setopt dired-listing-switches "-lah"
          dired-hide-details-hide-absolute-location t
          dired-do-revert-buffer t
          dired-dwim-target t
          dired-x-hands-off-my-keys nil
          dired-auto-revert-buffer t
          dired-mouse-drag-files t
          shell-command-prompt-show-cwd t))

(defun +dired-side-noselect ()
  "Open a dired buffer in a side window. "
  (interactive)
  (let ((buf (dired-noselect (and (project-current) (project-root (project-current))))))
    (with-current-buffer buf
      (make-local-variable 'display-buffer-alist)
      (cl-pushnew '((derived-mode . dired-mode) (display-buffer-same-window)) display-buffer-alist :test 'equal)
      (dired-hide-details-mode)
      (run-hooks '+dired-side-hook))
    buf))

(defun +dired-side ()
  (interactive)
  (display-buffer
   (+dired-side-noselect)
   `((display-buffer-in-side-window)
     (side . left)
     (dedicated . t)
     (window-width . 40)
     (window-parameters . ((dired-side . t))))))

(add-hook '+dired-side-hook (lambda () (tab-line-mode -1)))

(defun +dired-side-goto-current-file (frame)
  (with-selected-frame frame
    (when-let* ((file (buffer-file-name))
                (side-win (window-with-parameter 'dired-side  nil frame))
                (buf (window-buffer side-win)))
      (with-current-buffer buf
        (dired-insert-subdir (file-name-directory file))
        (dired-goto-file file)
        (recenter)
        (hl-line-highlight)))))

;; (add-hook 'window-buffer-change-functions #'+dired-side-goto-current-file)

(provide 'dotemacs-dired)
;;; dotemacs-dired.el ends here
