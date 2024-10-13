;;; debian-copyright.el --- Major mode for Debian package copyright files

;; Copyright 2002, 2003 Junichi Uekawa.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-copyright.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:
;; debian-copyright-mode will automatically activate when editing files
;; named debian/*copyright or /usr/share/doc/*/copyright.  This mode
;; can be customised from the group tools -> debian-copyright, or
;; directly from the debian-copyright group.

;;; Code:

(require 'debian-changelog-mode)

;;; Code:

(defgroup debian-copyright nil "Debian copyright mode"
  :group 'tools
  :prefix "debian-copyright-")

(defcustom debian-copyright-mode-load-hook nil
  "*Hooks that are run when `debian-copyright-mode' is loaded."
  :group 'debian-copyright
  :type 'hook)

(defcustom debian-copyright-mode-hook nil
  "Normal hook run when entering Debian Copyright mode."
  :group 'debian-copyright
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode))

(defconst debian-copyright-mode-version "$Id: debian-copyright.el,v 1.5 2010-07-28 15:33:45 psg Exp $" "Version of debian copyright mode.")

(defvar debian-copyright-mode-map nil
  "Keymap for debian/copyright mode.")
(defvar debian-copyright-mode-syntax-table nil
  "Syntax table for debian/copyright mode.")

(defvar debian-copyright-font-lock-keywords nil
  "Regexps to highlight in font-lock.")

(defvar debian-copyright--field-names
  '("Comment"
    "Copyright"
    "Disclaimer"
    "Files"
    "Format"
    "License"
    "Source"
    "Upstream-Contact"
    "Upstream-Name")
  "Supported field names in Debian copyright files.")

(defvar debian-copyright--supported-url-protocol-prefixes
  '("file:///"
    "ftp://"
    "git://"
    "http://"
    "https://"
    "ssh://"
    "mailto:")
  "Supported common protocol prefix in URLs.
It should be immediately followed by a non-slash character.")

(defvar debian-copyright--supported-licenses
  '("Apache-2\\.0"
    "Artistic"
    "BSD"
    "CC0-1\\.0"
    "GFDL"
    "GFDL-1\\.2"
    "GFDL-1\\.3"
    "GPL"
    "GPL-1"
    "GPL-2"
    "GPL-3"
    "LGPL"
    "LGPL-2"
    "LGPL-2\\.1"
    "LGPL-3"
    "MPL-1\\.1"
    "MPL-2\\.0")
  "Supported licenses based on /usr/share/common-licenses.")

(if debian-copyright-mode-syntax-table
    ()              ; Do not change the table if it is already set up.
  (setq debian-copyright-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " debian-copyright-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " debian-copyright-mode-syntax-table)
  (modify-syntax-entry ?' "w   " debian-copyright-mode-syntax-table))

(defun debian-copyright--font-lock-add-field-keywords (field-names)
  "Add font lock for field in FIELD-NAMES."
  (dolist (field-name field-names)
    (add-to-list 'debian-copyright-font-lock-keywords
                 `(,(concat "^" field-name ":") . font-lock-keyword-face))))

(defun debian-copyright--font-lock-add-email ()
  "Add font lock for email addresses.
This is not a fully compliant email detecting regexp.  It
additionally detects the extra `<' and `>' around the address.  I
hope this works well enough until the day we have to follow
https://stackoverflow.com/a/201378."
  (add-to-list 'debian-copyright-font-lock-keywords
               '("<?\\([^<> \t\n]+@[^<> \t\n]+\\.[^<> \t\n]+\\)>?"
                 (1 font-lock-variable-name-face))))

(defun debian-copyright--font-lock-add-urls (protocol-prefixes)
  "Add font lock for a URL with PROTOCOL-PREFIXES."
  (dolist (protocol-prefix protocol-prefixes)
    (add-to-list 'debian-copyright-font-lock-keywords
                 `(,(concat "\\(" protocol-prefix "[^/ \t\n][^ \t\n]*\\)")
                   (1 font-lock-constant-face)))))

(defun debian-copyright--font-lock-add-licenses (supported-licenses)
  "Add font lock for SUPPORTED-LICENSES."
  (dolist (supported-license supported-licenses)
    (add-to-list 'debian-copyright-font-lock-keywords
                 `(,(concat "^\\License:.*\\_<\\(" supported-license
                            "\\+?\\)\\_>")
                   (1 font-lock-type-face)))))

;;;###autoload
(defun debian-copyright-mode ()
  "Mode to edit and read debian/copyright.
\\{debian-copyright-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'debian-copyright-mode)
  (setq mode-name "debian/copyright")
  (mapc 'make-local-variable '(font-lock-defaults write-file-hooks))
  (use-local-map debian-copyright-mode-map)
  (set-syntax-table debian-copyright-mode-syntax-table)
  ;; Add font locks
  (debian-copyright--font-lock-add-field-keywords debian-copyright--field-names)
  (debian-copyright--font-lock-add-email)
  (debian-copyright--font-lock-add-licenses debian-copyright--supported-licenses)
  (defvar goto-address-highlight-p)  ;; To suppress comp warnings.
  (if (or (not (featurep 'goto-addr))
          (not goto-address-highlight-p))
      (debian-copyright--font-lock-add-urls
       debian-copyright--supported-url-protocol-prefixes)
    (goto-address))
  (setq font-lock-defaults
        '(debian-copyright-font-lock-keywords
          nil  ;keywords-only
          nil  ;case-fold
          ()   ;syntax-alist
          ))
  (run-mode-hooks 'debian-copyright-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("debian/.*copyright\\'" . debian-copyright-mode))
;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\`/usr/share/doc/.*/copyright" . debian-copyright-mode))

(run-hooks 'debian-copyright-mode-load-hook)

(provide 'debian-copyright)

;;; debian-copyright.el ends here
