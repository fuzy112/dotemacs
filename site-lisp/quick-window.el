;;; quick-window.el --- Quickly jump to a window -*- lexical-binding: t -*-
;; Copyright © 2024, 2025 Zhengyi Fu <i@fuzy.me>

;; Author:  Zhengyi Fu
;; Package-Requires: ((emacs "29.4") (posframe "1.4.4"))
;; Version: 0.3.0
;; Keywords: navigation

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

(require 'posframe)
(eval-when-compile (require 'cl-lib))

(defgroup quick-window nil
  "Quickly jump to a window."
  :group 'windows
  :prefix "quick-window-")

(defcustom quick-window-keys "htnsueoaid"
  "Letters used jump to windows."
  :type 'string)

(defface quick-window-label
  '((((class color) (min-colors 256)) :foreground "white" :background "red" :weight bold)
    (t :inherit highlight))
  "Face used to display window labels.")

(defun quick-window--before-p (w1 w2)
  (pcase-let ((`(,left-1 ,top-1 . ,_) (window-edges w1))
	      (`(,left-2 ,top-2 . ,_) (window-edges w2)))
    (or (< left-1 left-2)
	(and (= left-1 left-2)
	     (< top-1 top-2)))))

(defun quick-window--get-posframe-buffer (char)
  (let ((buf-name (format " *quick-window : %c*" char)))
    (or (get-buffer buf-name)
	(with-current-buffer (get-buffer-create buf-name)
	  (insert (propertize (format "[%c]" char)
			      'face 'quick-window-label))
	  (current-buffer)))))

;;;###autoload
(defun quick-window-jump (&optional arg)
  "Quickly jump to a window by assigned character labels.

By default, considers only the windows in the current frames.
Prefixed with one \\[universal-argument], considers all windows on all
visible frames.
Prefixed with two \\[universal-argument]'s, considers all windows on
visible and iconified frames.
Prefixed with three \\[universal-argument]'s, considers all windows on
all existing frames."
  (interactive "p")
  (let*
      ((all-frames (cond ((eq arg 4) 'visible)
			 ((eq arg 16) 0)
			 ((eq arg 64) t)
			 (t nil)))
       (windows (window-list-1 nil nil all-frames)))
    (if (length< windows 3)
	(cl-loop for win in windows
		 with current-win = (selected-window)
		 when (not (eq win current-win))
		 return (progn
			  (select-frame-set-input-focus (window-frame win))
			  (select-window win)))
      (let (posframes)
	(unwind-protect
	    (let (window-map)
	      (cl-loop with sorted-windows = (sort windows #'quick-window--before-p)
		       for win in sorted-windows
		       for letter being the elements of quick-window-keys
		       do (setq window-map (plist-put window-map letter win #'eql))
		       do (with-selected-window win
			    (posframe-show (quick-window--get-posframe-buffer letter)
					   :poshandler #'posframe-poshandler-window-top-left-corner))
		       do (push (quick-window--get-posframe-buffer letter) posframes))
	      (redisplay)
	      (let* ((key (read-key "Jump to window: "))
		     (win (plist-get window-map key #'eql)))
		(when (eql key ?\C-g)
		  (keyboard-quit))
		(unless win
		  (user-error "No such window: `%s'" (key-description (list key))))
		(unless (eq (window-frame win) (selected-frame))
		  (select-frame-set-input-focus (window-frame win)))
		(select-window win)))
	  (mapc #'posframe-hide posframes)))))
  (pulse-momentary-highlight-one-line))

(provide 'quick-window)
;;; quick-window.el ends here
