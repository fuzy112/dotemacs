;;; activity-persist-buffers.el --- Persistent activity buffers -*- lexical-binding: t -*-
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

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "29.3") (activities "0.8"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; `activities-persist-buffers-mode' is a global minor mode that persists
;; activity buffers across sessions.

;;; Code:

(require 'activities)
(require 'activities-tabs)

(cl-defstruct (activities-persist-buffers-activity-state
               (:include activities-activity-state))
  "activities-activity-state with buffers."
  buffer-list)

(defun activities-persist-buffers--error-buffer-p (buf)
  (buffer-match-p "\\`\\*Activities (error):" buf))

(defun activities-persist-buffers--backtrace-p (buf)
  (buffer-match-p '(derived-mode . backtrace-mode) buf))

(defvar activities-persist-buffers-anti-save-predicates
  '(activities-buffer-special-p
    activities-buffer-hidden-p
    activities-persist-buffers--error-buffer-p
    activities-persist-buffers--backtrace-p
    minibufferp))

(defun activities-persist-buffers--save-activity-buffers (activity-state)
  (make-activities-persist-buffers-activity-state
   :window-state (activities-activity-state-window-state activity-state)
   :etc (activities-activity-state-etc activity-state)
   :buffer-list
   (let ((buffer-list (activities-tabs--tab-parameter
                       'activities-buffer-list
                       (activities-tabs--tab (activities-current)))))
     (setq buffer-list (seq-filter #'buffer-live-p buffer-list))
     (setq buffer-list
           (seq-filter
            (lambda (buf)
              (not (run-hook-with-args-until-success
                    'activities-persist-buffers-anti-save-predicates buf)))
            buffer-list))
     (mapcar #'activity--serialize buffer-list))))

(defun activities-persist-buffers--restore-buffers (buffers)
  (let ((buffer-list (mapcar #'activities--deserialize buffers)))
    (let* ((tab (tab-bar--current-tab-find)))
      (setf (alist-get 'activities-buffer-list (cdr tab)) buffer-list))))

(cl-defun activities-persist-buffers--restore-activity-buffers (activity &key (state 'last))
  (activities-with activity
    (pcase-let (((cl-struct activities-activity name default last) activity))
      (pcase state
        ('default (activities-persist-buffers--restore-buffers (activities-persist-buffers-activity-state-buffer-list default)))
        ('last (if last
                   (activities-persist-buffers--restore-buffers (activities-persist-buffers-activity-state-buffer-list last))
                 (activities-persist-buffers--restore-buffers (activities-persist-buffers-activity-state-buffer-list default))))))))

;;;###autoload
(define-minor-mode activities-persist-buffers-mode
  "Activities-Persist-Buffers mode."
  :global t
  (advice-remove 'activities-state #'activities-persist-buffers--save-activity-buffers)
  (advice-remove 'activities-set #'activities-persist-buffers--restore-activity-buffers)
  (when activities-persist-buffers-mode
    (activities-tabs-mode)
    (advice-add 'activities-state :filter-return #'activities-persist-buffers--save-activity-buffers)
    (advice-add 'activities-set :after #'activities-persist-buffers--restore-activity-buffers)))

(provide 'activities-persist-buffers)
;;; activity-persist-buffers.el ends here
