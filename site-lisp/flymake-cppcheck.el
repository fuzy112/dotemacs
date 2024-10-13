;;; flymake-cppcheck.el --- CppCheck diagnostic function -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.5.0
;; Keywords: languages, c

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
