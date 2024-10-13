;;; autocrypt-mu4e.el --- Autocrypt for mu4e -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024  Free Software Foundation, Inc.

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

;; MUA specific functions for mu4e
;;
;; Setup with (add-hook 'mu4e-main-mode-hook #'autocrypt-mode)

;;; Code:

(require 'autocrypt)

(declare-function mu4e-view-raw-message "mu4e" ())

;;;###autoload
(defun autocrypt-mu4e--install ()
  "Install autocrypt hooks for mu4e."
  (add-hook (if (boundp 'mu4e-view-rendered-hook)
                'mu4e-view-rendered-hook
              'mu4e-view-mode-hook)
            #'autocrypt-process-header nil t)
  (add-hook 'mu4e-compose-mode-hook #'autocrypt-compose-setup nil t))

(defun autocrypt-mu4e--uninstall ()
  "Remove autocrypt hooks for mu4e."
  (remove-hook (if (boundp 'mu4e-view-rendered-hook)
                   'mu4e-view-rendered-hook
                 'mu4e-view-mode-hook)
               #'autocrypt-process-header t)
  (remove-hook 'mu4e-compose-mode-hook #'autocrypt-compose-setup t))

(defun autocrypt-mu4e--get-header (header)
  "Ask mu4e to return HEADER."
  (save-window-excursion
    (with-current-buffer (mu4e-view-raw-message)
      (prog1 (mail-fetch-field header)
        (kill-buffer (current-buffer))))))

(provide 'autocrypt-mu4e)

;;; autocrypt-mu4e.el ends here
