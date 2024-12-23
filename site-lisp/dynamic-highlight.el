;;; dynamic-highlight.el --- Highlight occurrences of symbol at point -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "31.0.50"))
;; Version: 0.1.0
;; Keywords: font-lock

;;; Commentary:

;; This is a local minor mode.  When activated, occurrences of the
;; symbol at point in the current buffer will be highlighted with
;; the `dynamic-highlight' face.
;;
;; To enable Dynamic Highlight mode in CC modes, add the following
;; code to your init.el:
;;
;; (add-hook \\='c-mode-common-hook #\\='dynamic-highlight-mode)
;;

;;; Code:

(require 'hi-lock)

(defgroup dynamic-highlight ()
  "Dynamic highlight."
  :prefix "dynamic-highlight-"
  :group 'programming)

(defface dynamic-highlight
  '((((class color grayscale) (min-colors 256) (background light))
     :weight bold :underline (:color "#333388" :position t))
    (((class color grayscale) (min-colors 256) (background dark))
     :weight bold :underline (:color "#9099d9" :position t))
    (t :inherit lazy-highlight))
  "Face used for highlight the symbol at point.")

(defcustom dynamic-highlight-delay 0.5
  "Number of seconds to delay before highlighting the symbol at point."
  :type 'number)

(defcustom dynamic-highlight-predicate #'always
  "Whether to highlight the symbol."
  :type 'function)

(defvar dynamic-highlight--timer nil)
(defvar-local dynamic-highlight--regexp nil)

(defun dynamic-highlight--update ()
  (and dynamic-highlight--regexp
       (hi-lock-unface-buffer dynamic-highlight--regexp)
       (setq dynamic-highlight--regexp nil))
  (when-let* (((symbol-value 'dynamic-highlight-mode))
	      (symbol (thing-at-point 'symbol))
	      ((funcall dynamic-highlight-predicate symbol)))
    (setq dynamic-highlight--regexp (regexp-opt (list symbol) 'symbols))
    (let ((hi-lock-use-overlays t))
      (hi-lock-set-pattern dynamic-highlight--regexp 'dynamic-highlight))))

;;;###autoload
(define-minor-mode dynamic-highlight-mode
  "Highlight all occurrences of the symbol at point."
  :global nil
  (when dynamic-highlight-mode
    (or dynamic-highlight--timer
	(setq dynamic-highlight--timer
	      (run-with-idle-timer
	       dynamic-highlight-delay t #'dynamic-highlight--update)))))

(provide 'dynamic-highlight)
;;; dynamic-highlight.el ends here
