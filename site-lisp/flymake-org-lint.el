;;; flymake-org-lint.el --- Org-lint flymake diagnostic function -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: languages, org

;;; Commentary:
;;; Code:

(require 'flymake)
(require 'org-lint)
(eval-when-compile (require 'cl-lib))

;;;###autoload(autoload 'flymake-org-lint "flymake-org-lint")
(defun flymake-org-lint (report-fn &rest _args)
  "Lint org buffer with `org-lint'.

REPORT-FN: see `flymake-diagnostic-functions'."
  (cl-loop for (_id data) in (org-lint)
           for line = (string-to-number (aref data 0))
           for description = (aref data 2)
           for region = (flymake-diag-region (current-buffer) line)
           collect (flymake-make-diagnostic
                    (current-buffer) (car region) (cdr region)
                    :warning
                    description)
           into diags
           finally (funcall report-fn diags)))

(provide 'flymake-org-lint)
;;; flymake-org-lint.el ends here
