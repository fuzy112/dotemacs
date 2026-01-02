;;; flymake-checkbashisms.el --- Flymake backend powered by checkbashisms -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.0
;; Keywords: tools, languages

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

;;;###autoload(autoload 'flymake-checkbashisms "flymake-checkbashisms")
(flymake-define flymake-checkbashisms
  :command ["checkbashisms" "--lint" "--extra" :input]
  :input :file
  :patterns
  ((:error   bol (file) ":" line ":" column ": error:"   (message) eol)
   (:warning bol (file) ":" line ":" column ": warning:" (message) eol)
   (:note    bol (file) ":" line ":" column ": note:"    (message) eol)))

(provide 'flymake-checkbashisms)
;;; flymake-checkbashisms.el ends here
