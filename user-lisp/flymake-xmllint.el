;;; flymake-xmllint.el --- xmllint diagnostic function -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.3.0
;; Keywords: languages, xml

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

;;;; Requirements

(require 'flymake-define)

;;;; Diagnostic function

;;;###autoload(autoload 'flymake-xmllint "flymake-xmllint")
(flymake-define flymake-xmllint
  :command ["xmllint" "--noout" "-"]
  :input :stdin
  :patterns
  ((:error bol (file) ":" line ":" (message) eol)))

(provide 'flymake-xmllint)
;;; flymake-xmllint.el ends here

