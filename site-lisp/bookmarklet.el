;;; bookmarklet.el --- Bookmarklet creation -*- lexical-binding: t -*-
;; Copyright © 2024 Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: convenient

;;; Commentary:

;; This file contains facility to convert arbitrary JavaScript code to
;; a bookmarklet that can be added to the bookmark tool bar of FireFox
;; (or Chromium).

;;; Code:

(require 'url-util)

(defun bookmarklet-from-string (code)
  (concat "javascript:void((function(){" (url-hexify-string code) "})())"))

;;;###autoload
(defun bookmarklet-from-buffer ()
  (interactive)
  (let ((str (bookmarklet-from-string (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer (get-buffer-create "*bookmarklet*")
      (erase-buffer)
      (insert str)
      (pop-to-buffer (current-buffer)))))

(provide 'bookmarklet)
;;; bookmarklet.el ends here
