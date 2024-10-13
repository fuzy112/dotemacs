;;; embark-bookmark.el --- Embark integration for bookmark -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:
;;; Code:

(require 'embark)
(require 'bookmark)

(defun embark-bookmark--bmenu-bookmark ()
  "Find bookmark entry in the current line in bookmark list mode."
  (save-restriction
    (ignore-errors
      (and-let* ((bookmark (bookmark-bmenu-bookmark)))
	`(bookmark ,bookmark ,(line-beginning-position) . ,(line-end-position))))))

;;;###autoload
(defun embark-bookmark--bmenu-setup ()
  (add-hook 'embark-target-finders #'embark-bookmark--bmenu-bookmark nil t))

;;;###autoload
(add-hook 'bookmark-bmenu-mode-hook #'embark-bookmark--bmenu-setup)

(provide 'embark-bookmark)
;;; embark-bookmark.el ends here
