;; -*- lexical-binding: t; -*-

;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

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

(after-load! password-store
  (add-hook 'savehist-save-hook #'password-store-clear))

(defun eat-send-pass nil
  "Send a password from Password-Store to the terminal."
  (interactive)
  (if eat-terminal nil
    (user-error "Process not running"))
  (require 'password-store)
  (password-store-get
   (completing-read "Password-store entry: " nil 'require-match t)
   (let ((my-term eat-terminal))
     (lambda (password)
       (eat-term-send-string my-term password)
       (eat-self-input 1 'return)))))
