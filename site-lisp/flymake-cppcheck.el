;;; flymake-cppcheck.el --- CppCheck diagnostic function -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.5.0
;; Keywords: languages, c

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

(defun flymake-cppcheck-command ()
  (if (derived-mode-p 'c-mode)
      ["cppcheck" "-q" "--language=c" :input]
    ["cppcheck" "-q" "--language=c++" :input]))

;;;###autoload(autoload 'flymake-cppcheck "flymake-cppcheck")
(flymake-define flymake-cppcheck
  :command #'flymake-cppcheck-command
  :input :file
  :debug nil
  :patterns
  ;; The regexp is copied from flymake-cc
  [(:error   line-start (file) ":" line ":" column ": " "error: " (message))
   (:warning line-start (file) ":" line ":" column ": " "warning" (message))
   (:note    line-start (file) ":" line ":" column ": " "note"    (message))])

(provide 'flymake-cppcheck)
;;; flymake-cppcheck.el ends here
