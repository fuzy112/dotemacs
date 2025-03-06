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

(cl-defstruct (activities-activity-state-with-buffers
               (:include activities-activity-state))
  "activities-activity-state with buffers."
  buffer-list)

(defun +activities-state--with-buffers (activity-state)
  (make-activities-activity-state-with-buffers
   :window-state (activities-activity-state-window-state activity-state)
   :etc (activities-activity-state-etc activity-state)
   :buffer-list
   (let ((buffer-list (activities-tabs--tab-parameter
                       'activities-buffer-list
                       (activities-tabs--tab (activities-current)))))
     (setq buffer-list (seq-filter #'buffer-live-p buffer-list))
     (mapcar (lambda (buf)
               (with-current-buffer buf
                 (activity--serialize buf)))
             buffer-list))))

(defun +activities--restore-buffers (buffers)
  (let ((buffer-list (mapcar #'activities--deserialize buffers)))
    (let* ((activity (activities-current))
           (tab (activities-tabs--tab activity)))
      (setf (alist-get 'activities-buffer-list (cdr tab)) buffer-list))))

(cl-defun +activities--restore-activity-buffers (activity &key (state 'last))
  (activities-with activity
    (pcase-let (((cl-struct activities-activity name default last) activity))
      (pcase state
        ('default (+activities--restore-buffers (activities-activity-state-with-buffers-buffer-list default)))
        ('last (if last
                   (+activities--restore-buffers (activities-activity-state-with-buffers-buffer-list last))
                 (+activities--restore-buffers (activities-activity-state-with-buffers-buffer-list default))))))))

;;;###autoload
(define-minor-mode activities-persist-buffers-mode
  "Activities-Persist-Buffers mode."
  :global t
  (advice-remove 'activities-state #'+activities-state--with-buffers)
  (advice-remove 'activities-set #'+activities--restore-activity-buffers)
  (when activities-persist-buffers-mode
    (activities-tabs-mode)
    (advice-add 'activities-state :filter-return #'+activities-state--with-buffers)
    (advice-add 'activities-set :after #'+activities--restore-activity-buffers)))

(provide 'activities-persist-buffers)
;;; activity-persist-buffers.el ends here
