;;; flymake-systemd.el --- Flymake diagnostic function for systemd unit files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: tools
;; Version: 0.3.0

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

;; This package provides a flymake diagnostic function that can check
;; syntax of systemd units files using the `systemd-analyze verify'
;; command.

;;; Code:

(require 'flymake-define)

;;;###autoload(autoload 'flymake-systemd "flymake-systemd")
(flymake-define flymake-systemd
  :documentation "Verify the systemd unit file."
  :command ("systemd-analyze" "verify" :input)
  :input :file
  :patterns ((:error bol (file) ": " (message) eol)
             (:error bol (file) ":" line ": " (message) eol)))

(provide 'flymake-systemd)
;;; flymake-systemd.el ends here
