;;; flymake-gnumake.el --- Flymake diagnostic function for GNU Make -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu

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

;;;###autoload(autoload 'flymake-gnumake "flymake-gnumake")
(flymake-define flymake-gnumake
  :documentation "Flymake diagnostic function for GNU Make.

This function runs `gmake --dry-run' on the buffer and provides
diagnostics."
  :command ("gmake" "--dry-run" "-f" :input)
  :input :file
  :patterns
  [(:error bol (file) ":" line ": " (message) eol)
   (:note  bol (? "gmake: ") "***" (message) eol)])

(provide 'flymake-gnumake)
;;; flymake-gnumake.el ends here
