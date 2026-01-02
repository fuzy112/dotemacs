;;; flymake-clang-format.el --- Clang-format diagnostic function  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: c

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

;; 

;;; Code:

(require 'flymake-define)

;;;###autoload(autoload 'flymake-clang-format "flymake-clang-format")
(flymake-define flymake-clang-format
  :condition (locate-dominating-file default-directory ".clang-format")
  :command ["clang-format" "--assume-filename" :input "--dry-run"]
  :input :stdin
  :patterns
  ((:warning bol (file) ":" line ":" column ": " "warning: " (message) eol)
   (:error bol (file) ":" line ":" column ": " "error: " (message) eol)
   (:note bol (file) ":" line ":" column ": " "note: " (message) eol)))

(provide 'flymake-clang-format)
;;; flymake-clang-format.el ends here
