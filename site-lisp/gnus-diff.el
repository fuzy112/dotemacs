;;; gnus-diff.el --- Highlight diffs in Gnus buffers -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: comm, prog

;;; Commentary:

;; This package provides support for highlighting patches inlined in
;; plain text emails in Gnus article buffers.

;;; Code:

(require 'ansi-color)

;;;###autoload
(defvar gnus-treat-highlight-diffs '(typep "text/plain"))

;;;###autoload
(defun gnus-article-highlight-diffs ()
  "Highlight the accessible part of the buffer using `delta (1)'."
  (interactive nil gnus-article-mode)
  (when-let ((delta-program (executable-find "delta"))
             (start (point-min))
             (article-buffer (current-buffer))
             (color-buffer (get-buffer-create " *delta-color*")))
    (with-current-buffer color-buffer
      (erase-buffer))
    (call-process-region (point-min) (point-max)
                         delta-program nil color-buffer nil
                         "--color-only")
    (with-current-buffer color-buffer
      (if (fboundp 'xterm-color-colorize-buffer)
          (xterm-color-colorize-buffer 'use-overlays)
        (let ((ansi-color-apply-face-function
               #'ansi-color-apply-overlay-face))
          (ansi-color-apply-on-region (point-min) (point-max))))
      (dolist (ol (overlays-in (point-min) (point-max)))
        (move-overlay ol
                      (+ start (- (overlay-start ol) (point-min)))
                      (+ start (- (overlay-end ol) (point-min)))
                      article-buffer))
      (erase-buffer))))

(defvar gnus-treatment-function-alist)

;;;###autoload
(with-eval-after-load 'gnus-art
  (add-to-list 'gnus-treatment-function-alist
	       '(gnus-treat-highlight-diffs gnus-article-highlight-diffs) 'append))

(provide 'gnus-diff)
;;; gnus-diff.el ends here

