;; -*- lexical-binding: t; -*-

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

(defcustom dynamic-highlight-predicate
  (lambda () (derived-mode-p 'prog-mode))
  "Wether to highlight the symbol."
  :type 'function)

(defvar dynamic-highlight--timer nil)

(defvar dynamic-highlight--regexp nil)
(defvar dynamic-highlight--buffer nil)

(defun dynamic-highlight--update ()
  (and dynamic-highlight--regexp
       dynamic-highlight--buffer
    (with-current-buffer dynamic-highlight--buffer
      (hi-lock-unface-buffer dynamic-highlight--regexp)
      (setq dynamic-highlight--regexp nil)))
  (when-let* ((symbol (thing-at-point 'symbol))
	      ((funcall dynamic-highlight-predicate)))
    (setq dynamic-highlight--regexp (regexp-opt (list symbol) 'symbols)
	  dynamic-highlight--buffer (current-buffer))
    (let ((hi-lock-use-overlays t))
      (hi-lock-set-pattern dynamic-highlight--regexp 'dynamic-highlight))))

;;;###autoload
(define-minor-mode dynamic-highlight-mode
  "Highlight all occurrences of the symbol at point."
  :global t
  (when (timerp dynamic-highlight--timer)
    (cancel-timer dynamic-highlight--timer))
  (when dynamic-highlight-mode
    (setq dynamic-highlight--timer
	  (run-with-idle-timer
	   dynamic-highlight-delay t #'dynamic-highlight--update))))

(provide 'dynamic-highlight)
;;; dynamic-highlight.el ends here
