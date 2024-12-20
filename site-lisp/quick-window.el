;;; quick-window.el --- Quickly jump to a window -*- lexical-binding: t -*-
;; Copyright © 2024 Zhengyi Fu <i@fuzy.me>

;; Author:  Zhengyi Fu
;; Package-Requires: ((emacs "29.4"))
;; Version: 0.1.0
;; Keywords: navigation

;;; Commentary:
;;; Code:

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

;;;###autoload
(defun quick-window-jump ()
  "Quickly jump to a window by assigned character labels."
  (interactive)
  (let ((windows (window-list)))
    (if (length< windows 3)
	(cl-loop for win in windows
		 with current-win = (selected-window)
		 when (not (eq win current-win))
		 return (select-window win))
      (let (overlays)
	(unwind-protect
	    (let (window-map)
	      (cl-loop with sorted-windows = (sort windows #'quick-window--before-p)
		       for win in sorted-windows
		       for letter being the elements of quick-window-keys
		       do (setq window-map (plist-put window-map letter win #'eql))
		       for start = (window-start win)
		       for ov = (make-overlay start start (window-buffer win))
		       do (push ov overlays)
		       do (overlay-put ov 'after-string
				       (propertize (format "[%c]" letter)
						   'face 'quick-window-label))
		       do (overlay-put ov 'window win))
	      (let ((key (read-key "Jump to window: ")))
		(when-let ((win (plist-get window-map key #'eql)))
		  (select-window win))))
	  (mapc #'delete-overlay overlays)))))
  (pulse-momentary-highlight-one-line))

(provide 'quick-window)
;;; quick-window.el ends here
