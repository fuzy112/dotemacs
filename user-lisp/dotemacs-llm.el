;;; dotemacs-llm.el --- LLM configuration -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

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

(eval-when-compile (require 'dotemacs-core))

(after-load! gptel
  (require 'gptel-config))

(after-load! agent-shell
  (defun agent-shell--init-comint-input-ring ()
    (let ((history-dir (agent-shell--dot-subdir "history")))
      (setq-local comint-input-ring-file-name (expand-file-name "comint-history.eld" history-dir))
      (comint-read-input-ring)
      (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)))
  (add-hook 'agent-shell-mode-hook #'agent-shell--init-comint-input-ring))

(provide 'dotemacs-llm)
;;; dotemacs-llm.el ends here
