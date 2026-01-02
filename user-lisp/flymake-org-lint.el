;;; flymake-org-lint.el --- Org-lint flymake diagnostic function -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Keywords: languages, org

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
