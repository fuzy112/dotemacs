;;; flymake-jq.el --- Flymake diagnostic function for JSON that utilizes jq -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.4.0
;; Keywords: tools

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

(require 'flymake-define)

;;;###autoload(autoload 'flymake-jq "flymake-jq")
(flymake-define flymake-jq
  :documentation "Flymake diagnostic function for JSON.

This functions runs `jq (1)' on the buffer to do syntax check."
  :command ("jq" "null")
  :input :stdin
  :patterns
  ((:error bol "parse error: " (message (*? anychar)) " at line " line ", column " column eol)))

(provide 'flymake-jq)
;;; flymake-jq.el ends here
