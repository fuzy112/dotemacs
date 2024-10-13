;;; flymake-checkbashisms.el --- Flymake backend powered by checkbashisms -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.0
;; Keywords: tools, languages

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
