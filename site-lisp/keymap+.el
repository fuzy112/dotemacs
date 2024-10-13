;;; keymap+.el --- Keymap utility                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
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

;; Extensions to the `keymap' interface.

;;; Code:

(require 'keymap)
(eval-when-compile '(require cl-lib))

;;;###autoload
(defmacro keymap-set-many (map &rest args)
  "Bind each KEY to DEF in MAP.

\(fn MAP [KEY DEF]...)"
  (declare (indent 1))
  (cl-once-only (map)
    (macroexp-progn
     (cl-loop for (key def) on args by #'cddr
	      collect `(keymap-set ,map ,key ,(if (symbolp def)
						  `(function ,def)
						`(function ,(eval def))))))))

;;;###autoload
(defmacro keymap-global-set-many (&rest args)
  "Bind each KEY to DEF.

\(fn [KEY DEF] ...)"
  (declare (indent 0))
  (macroexpand `(keymap-set-many (current-global-map) ,@args)))

;;;###autoload
(defun keymap-autoload (map keys thunk)
  "Autoload KEYS in MAP with BODY."
  (let ((def (lambda ()
	       (interactive)
	       (let ((k (this-command-keys)))
		 (funcall thunk)
		 (setq unread-command-events
		       (cons (mapcar (lambda (ev) (cons t ev))
				     (listify-key-sequence k))
			     unread-command-events))))))
    (seq-doseq (k keys)
      (keymap-set map k def))))

;;;###autoload
(defun keymap-set-prefix-map (map key prefix-map file)
  "Autoload and bind KEY in MAP to PREFIX-MAP.

PREFIX-MAP must be defined in FILE."
  (if (boundp prefix-map)
      (keymap-set map key (symbol-value prefix-map))
    (keymap-autoload map
      (list key)
      (lambda ()
	(load file)
	(keymap-set map key (symbol-value prefix-map))))))

;;;###autoload
(defun keymap-global-set-prefix-map (key prefix-map file)
  "Autoload and bind KEY to PREFIX-MAP.

See `keymap-set-prefix-map'."
  (keymap-set-prefix-map (current-global-map) key prefix-map file))


;;;###autoload
(defsubst keymap-activate-global-minor-mode (mode &rest keys)
  "Lazy-enable MODE when any of KEYS is pressed."
  (declare (indent 1))
  (if (autoloadp (symbol-function mode))
      (keymap-autoload
	  (current-global-map)
	keys
	(lambda () (funcall mode)))
    (funcall mode)))

(provide 'keymap+)
;;; keymap+.el ends here
