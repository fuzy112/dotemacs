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
(after-load! auth-source
  (require! auth-source-pass)
  (when (custom--standard-value-p 'auth-sources auth-sources)
    (setopt auth-sources '("~/.authinfo.gpg" password-store)))
  (setopt auth-source-save-behavior t
          auth-source-gpg-encrypt-to (list "0xBBE2757FC7BFC23B"))
  (setopt plstore-encrypt-to auth-source-gpg-encrypt-to)
  (auth-source-forget-all-cached)
  (keymap-set read-passwd-map "C-c C-p" #'+insert-pass))

;;;; epg

(after-load! epa
  (setopt epa-keys-select-method 'minibuffer))

;;;; plstore

(after-load! plstore
  (setopt plstore-select-keys nil))


(declare-function auth-source-pass-search "auth-source-pass")
(declare-function auth-source-pass-entries "auth-source-pass")
(defun +insert-pass ()
  (interactive)
  (require 'auth-source-pass)
  (when-let* ((entry (completing-read
                      "Select pass entry: "
                      (auth-source-pass-entries)))
              (info (auth-source-pass-search :host entry)))
    (cl-assert (length= info 1))
    (insert (auth-info-password (car info)))))

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
