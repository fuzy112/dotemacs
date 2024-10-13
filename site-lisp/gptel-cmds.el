;;; gptel-cmds.el --- Commands based on Gptel        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

;; Author:  Zhengyi Fu <i@fuzy.me>
;; Keywords: convenience, tools, comm

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

;; 

;;; Code:

(require 'gptel)
(require 'gptel-quick)

;;;; Translate

(defun gptel-translate (query-text)
  (interactive
   (list (cond
	  ((use-region-p) (buffer-substring-no-properties (region-beginning)
							  (region-end)))
	  ((and (derived-mode-p 'pdf-view-mode)
		(pdf-view-active-region-p))
	   (mapconcat #'identity (pdf-view-active-region-text) "\n\n"))
	  (t (thing-at-point 'sexp)))))
  (when (xor gptel-quick-backend gptel-quick-model)
    (error "gptel-quick-backend and gptel-quick-model must be both set or unset"))
  (let* ((gptel-use-curl)
	 (gptel-max-tokens (* 2.5 (length query-text)))
	 (gptel-use-context (and gptel-quick-use-context 'system))
	 (gptel-backend gptel-ollama)
	 (gptel-model "qwen2.5:latest"))
    (gptel-request query-text
      :system (format "Translate into Chinese.")
      :context (list query-text 4
		     (posn-at-point (and (use-region-p) (region-beginning))))
      :callback #'gptel-quick--callback-posframe)))

(provide 'gptel-cmds)
;;; gptel-cmds.el ends here
