;;; check-deps.el --- Check external dependencies -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
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

(require 'compat nil t)

(defvar check-deps-required-programs
  '(("git" ("--version") "^git version \\(.+\\)$" "2.34.0")
    ("unshare" ("--version") "^unshare from util-linux \\(.+\\)$" "2.10")
    ("cppcheck" ("--version") "^Cppcheck \\(.+\\)$" "2.7")
    ("clang-tidy" ("--version") "LLVM version \\(.+\\)$" "14.0.0")
    ("clang-format" ("--version") "clang-format version \\(.+\\)$" "14.0.0")
    ("xmllint" ("--version") "using libxml version \\(.+\\)$" "20913")
    ("jq" ("--version") "^jq-\\(.+\\)$" "1.6")
    ("cmake" ("--version") "^cmake version \\(.+\\)$" "3.16.0")
    ("find" ("--version") "^find (GNU findutils) \\(.+\\)$" "4.5.0")
    ("grep" ("--version") "^grep (GNU grep) \\(.+\\)$" "3.6")
    ("ctags" ("--version") "^Universal Ctags \\([^,]+\\)," "5.9.0")
    ("gpg" ("--version") "^gpg (GnuPG) \\(.+\\)$" "2.2.27")
    ("pass" ("--version") "\\_<v\\([^ ]+\\) " "1.7.4")
    ("gmake" ("--version") "^GNU Make \\(.+\\)$" "4.3")
    ("checkbashisms" ("--version") "version \\([0-9.]+\\)" "2.22.1")
    ("verilator" ("--version") "^Verilator \\([^ ]+\\) " "5.006")
    ("node" ("--version") "^v\\(.*\\)" "18")))

;;;###autoload
(defun check-deps ()
  (interactive)
  (let* ((buf (get-buffer-create "*requirements*"))
         (insert-line (lambda (&rest args)
                        (with-current-buffer buf
                          (apply #'insert args)
                          (newline)))))
    (with-current-buffer buf
      (unless (equal (point-min) (point-max))
        (goto-char (point-max))
        (insert "\n\n")))
    (dolist (req check-deps-required-programs)
      (let ((prog (nth 0 req))
            (args (nth 1 req))
            (re (nth 2 req))
            (minver (nth 3 req))
            exe ver)
        (if (setq exe (executable-find prog))
            (with-temp-buffer
              (if (not (zerop (apply #'call-process exe nil t nil args)))
                  (funcall insert-line prog ": error: "
                           (progn
                             (goto-char (point-min))
                             (buffer-substring-no-properties
                              (point-min) (line-end-position))))
                (goto-char (point-min))
                (if (re-search-forward re nil t)
                    (progn
                      (setq ver (match-string 1))
                      (funcall insert-line prog ": found " exe
                               ", version: " ver
                               ", required version: " minver
                               ", ok: "
                               (if (string-version-lessp ver minver)
                                   "no"
				 "yes")))
                  (funcall insert-line prog ": found " exe
                           ", version not found"
                           ", required-version: " minver
                           ", ok: no"))))
          (funcall insert-line prog ": not found"))))
    (funcall insert-line)
    (pop-to-buffer buf)
    (goto-char (point-max))))

