;;; debian-autopkgtest-control-mode.el --- Major mode for Debian package autopkgtest control files

;; Copyright 2024 Xiyue Deng <manphiz@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-autopkgtest-control-mode.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:

;; debian-autopkgtest-control-mode will automatically activate when editing
;; files named debian/tests/control.  This mode can be customised from the group
;; tools -> debian-autopkgtest-control-mode, or directly from the
;; debian-autopkgtest-control-mode group.

;; The author learned many from debian-copyright.el by Junichi Uekawa during the
;; development.

;;; Code:

(defgroup debian-autopkgtest-control-mode nil "Debian autopkgtest control mode"
  :group 'tools
  :prefix "debian-autopkgtest-control-mode-")

(defcustom debian-autopkgtest-control-mode-load-hook nil
  "*Hooks that are run when `debian-autopkgtest-control-mode' is loaded."
  :group 'debian-autopkgtest-control-mode
  :type 'hook)

(defcustom debian-autopkgtest-control-mode-hook nil
  "Normal hook run when entering Debian Copyright mode."
  :group 'debian-autopkgtest-control-mode
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode))

(defvar debian-autopkgtest-control-mode-map nil
  "Keymap for debian-autopkgtest-control-mode.")
(defvar debian-autopkgtest-control-mode-syntax-table nil
  "Syntax table for debian-autopkgtest-control-mode.")

(defvar debian-autopkgtest-control-mode-font-lock-keywords nil
  "Regexps to highlight in font-lock.")

(defvar debian-autopkgtest-control-mode--field-names
  '("Architecture"
    "Classes"
    "Depends"
    "Features"
    "Restrictions"
    "Test-Command"
    "Tests"
    "Tests-Directory")
  "Supported field names in Debian autopkgtest control files.
Using fields as defined in
https://people.debian.org/~eriberto/README.package-tests.html.")

(defvar debian-autopkgtest-control-mode--restrictions
  '("allow-stderr"
    "breaks-testbed"
    "build-needed"
    "flaky"
    "hint-testsuite-triggers"
    "isolation-container"
    "isolation-machine"
    "needs-internet"
    "needs-reboot"
    ;; "needs-recommends" (deprecated)
    "needs-root"
    "needs-sudo"
    "rw-build-tree"
    "skip-not-installable"
    "skippable"
    "superficial")
  "Restrictions for autopkgtest.
Using the restriction list as defined in
https://people.debian.org/~eriberto/README.package-tests.html.")

(defvar debian-autopkgtest-control-mode--dependency-extensions
  '("@"
    "@builddeps@"
    "@recommends@")
  "Build dependency extensions for autopkgtest.
As defined in the `Depends' section in
https://people.debian.org/~eriberto/README.package-tests.html.")

(if debian-autopkgtest-control-mode-syntax-table
    ()              ; Do not change the table if it is already set up.
  (setq debian-autopkgtest-control-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " debian-autopkgtest-control-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " debian-autopkgtest-control-mode-syntax-table)
  (modify-syntax-entry ?' "w   " debian-autopkgtest-control-mode-syntax-table))

(defun debian-autopkgtest-control-mode--font-lock-add-field-keywords (field-names)
  "Add font lock for field in FIELD-NAMES."
  (dolist (field-name field-names)
    (add-to-list 'debian-autopkgtest-control-mode-font-lock-keywords
                 `(,(concat "^" field-name ":") . font-lock-keyword-face))))

(defun debian-autopkgtest-control-mode--font-lock-add-dependency-extensions
    (extensions)
  "Add font lock for dependency EXTENSIONS."
  (dolist (extension extensions)
    (add-to-list 'debian-autopkgtest-control-mode-font-lock-keywords
                 `(,extension . font-lock-variable-name-face))))

(defun debian-autopkgtest-control-mode--font-lock-add-restrictions (restrictions)
  "Add font lock for RESTRICTIONS."
  (dolist (restriction restrictions)
    (add-to-list 'debian-autopkgtest-control-mode-font-lock-keywords
                 `(,(concat "^\\Restrictions:.*\\_<\\(" restriction
                            "\\)\\_>")
                   (1 font-lock-type-face)))))

;;;###autoload
(defun debian-autopkgtest-control-mode ()
  "Mode to edit and read debian-autopkgtest-control-mode.
\\{debian-autopkgtest-control-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'debian-autopkgtest-control-mode)
  (setq mode-name "debian-autopkgtest-control")
  (mapc 'make-local-variable '(font-lock-defaults write-file-hooks))
  (use-local-map debian-autopkgtest-control-mode-map)
  (set-syntax-table debian-autopkgtest-control-mode-syntax-table)
  ;; Add font locks
  (debian-autopkgtest-control-mode--font-lock-add-field-keywords
   debian-autopkgtest-control-mode--field-names)
  (debian-autopkgtest-control-mode--font-lock-add-dependency-extensions
   debian-autopkgtest-control-mode--dependency-extensions)
  (debian-autopkgtest-control-mode--font-lock-add-restrictions
   debian-autopkgtest-control-mode--restrictions)
  (setq font-lock-defaults
        '(debian-autopkgtest-control-mode-font-lock-keywords
          nil  ;keywords-only
          nil  ;case-fold
          nil  ;syntax-alist
          ))
  (run-mode-hooks 'debian-autopkgtest-control-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("debian/tests/control\\'" . debian-autopkgtest-control-mode))

(run-hooks 'debian-autopkgtest-control-mode-load-hook)

(provide 'debian-autopkgtest-control-mode)

;;; debian-autopkgtest-control-mode.el ends here
