;;; autocrypt-rmail.el --- Autocrypt for Rmail -*- lexical-binding:t -*-

;; Copyright (C) 2020-2023  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>

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

;; MUA specific functions for Rmail
;;
;; Setup with (add-hook 'rmail-mode-hook #'autocrypt-mode)

;;; Code:

(require 'autocrypt)
(require 'rmail)

(defun autocrypt-rmail--install ()
  "Install autocrypt functions into the current rmail buffer."
  (add-hook 'rmail-show-message-hook #'autocrypt-process-header nil t))

(defun autocrypt-rmail--uninstall ()
  "Remove autocrypt from current buffer."
  (add-hook 'rmail-show-message-hook #'autocrypt-process-header t))

(defun autocrypt-rmail--get-header (header)
  "Ask Rmail to return HEADER."
  (rmail-apply-in-message
   rmail-current-message
   (lambda () (mail-fetch-field header))))

(provide 'autocrypt-rmail)

;;; autocrypt-rmail.el ends here
