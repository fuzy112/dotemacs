;;; flymake-verilator.el --- Verilator diagnostic function -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.4.0
;; Keywords: tools

;; This file is not part of GNU Emacs.

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

(flymake-define flymake-verilator
  :documentation "Lint the verlog code."
  :command ("verilator" "--timing" "--lint-only" :input)
  :input :file
  :patterns
  ((:error bol "%Error: " (file) ":" line ":" column ": " (message) eol)))

(provide 'flymake-verilator)
;;; flymake-verilator.el ends here
