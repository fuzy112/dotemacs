;; telega-config.el -- Telega configuration -*- lexical-binding: t; -*-

;; Copyright © 2024-2025  Zhengyi Fu <i@fuzy.me>

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

(require 'dotemacs-core)
(require 'telega-customize)

(setq telega-server-command (expand-file-name "telega-server"
                                              (file-name-directory (locate-library "telega"))))
(setq telega-translate-to-language-by-default "zh")
(setq telega-emoji-use-images nil)

;; (defun telega-config-workaround-gaps-on-graphic-display ()
;;   "Enable avatar gap workaround when running on a graphic display."
;;   (when (display-graphic-p)
;;     (setq-local telega-avatar-workaround-gaps-for '(return t))))
;; (add-hook 'telega-chat-mode-hook #'telega-config-workaround-gaps-on-graphic-display)

(declare-function telega-chat-auto-fill-mode "telega-chat.el" (&optional arg1))
(add-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)

(after-load! telega
  (telega-notifications-mode))

(after-load! consult
  (add-to-list 'consult-buffer-filter (concat "\\`" (regexp-quote (telega-symbol 'telegram))))
  (add-to-list 'consult-buffer-filter "\\`\\*Telega"))

(defvar telega-cache-dir)
(define-advice telega (:around (&rest args) default-directory)
  (let ((default-directory "~/"))
    (apply args)))

(provide 'telega-config)
;;; telega-config.el ends here
