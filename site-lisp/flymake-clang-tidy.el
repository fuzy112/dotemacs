;;; flymake-clang-tidy.el --- Clang-tidy diagnostic function-*- lexical-binding: t -*-
;; Copyright © 2024 Zhengyi Fu

;; Author:   Zhengyi Fu
;; Version: 0.1.0
;; Keywords: languages, c

;;; Commentary:
;;; Code:

(require 'flymake-define)

;;;###autoload(autoload 'flymake-clang-tidy "flymake-clang-tidy")
(flymake-define flymake-clang-tidy
  :command ["clang-tidy" :input]
  :input :file
  :patterns
  ((:warning bol (file) ":" line ":" column ": " "warning: " (message) eol)
   (:error bol (file) ":" line ":" column ": " "error: " (message) eol)
   (:note bol (file) ":" line ":" column ": " "note: " (message) eol)))

(provide 'flymake-clang-tidy)
;;; flymake-clang-tidy.el ends here
