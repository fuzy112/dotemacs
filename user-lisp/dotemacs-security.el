;;; dotemacs-security.el  -*- lexical-binding: t; -*-

;; Copyright © 2024-2026  Zhengyi Fu <i@fuzy.me>

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


(eval-when-compile (require 'dotemacs-core))

;;;; auth-sources
(after-load! auth-sources
  (when (custom--standard-value-p 'auth-sources auth-sources)
    (setopt auth-sources '("~/.authinfo.gpg")))
  (setopt auth-source-save-behavior t
          auth-source-gpg-encrypt-to (list  "0xBBE2757FC7BFC23B"))
  (auth-source-forget-all-cached))

;;;; epg

(after-load! epa
  (setopt epa-keys-select-method 'minibuffer))

(defun send-password-to-process (process)
  "Read a password and send it to the process in BUFFER."
  (interactive
   (list
    (if-let* ((proc (get-buffer-process (current-buffer)))
              ((not current-prefix-arg)))
        proc
      (read-process-name "Process"))))
  (process-send-string process
                       (concat
                        (read-passwd "Password: ")
                        "\n")))

(provide 'dotemacs-security)
;;; dotemacs-security.el ends here
