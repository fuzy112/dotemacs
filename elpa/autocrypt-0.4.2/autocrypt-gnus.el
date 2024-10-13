;;; autocrypt-gnus.el --- Autocrypt for Gnus -*- lexical-binding:t -*-

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

;; MUA specific functions for Gnus
;;
;; Setup with (add-hook 'gnus-load-hook #'autocrypt-mode)

;;; Code:

(require 'gnus)
(require 'gnus-art)
(require 'mm-decode)
(require 'autocrypt)

;;;###autoload
(defun autocrypt-gnus--install ()
  "Prepare autocrypt for Gnus."
  (add-hook 'gnus-select-article-hook #'autocrypt-process-header nil t))

(defun autocrypt-gnus--uninstall ()
  "Undo `autocrypt-gnus--install'."
  (remove-hook 'gnus-article-prepare-hook #'autocrypt-process-header t))

(defun autocrypt-gnus--get-header (header)
  "Return value for HEADER from current message."
  (gnus-fetch-original-field header))

(defun autocrypt-gnus--get-part (index)
  "Return the INDEX'th part of the message as a string."
  (save-window-excursion
    (let ((gnus-inhibit-mime-unbuttonizing t)
          (content nil))
      (condition-case nil
          (gnus-article-part-wrapper
           (1+ index)
           (lambda (&optional handle _arg _event)
             (unless handle
               (gnus-article-jump-to-part (1+ index))
               (setq handle (get-text-property (point) 'gnus-data)))
             (with-temp-buffer
               (mm-insert-part handle)
               (setq content (buffer-string)))))
        (error))
      content)))

(provide 'autocrypt-gnus)

;;; autocrypt-gnus.el ends here
