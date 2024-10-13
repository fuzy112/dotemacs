;;; debian-el.el --- startup file for the debian-el package

;; Author: Debian Emacsen Team <debian-emacsen@lists.debian.org>
;; Version: 37.16-git
;; Keywords: debian apt elisp

;; This file is not part of GNU Emacs.

;; debian-el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; debian-el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides emacs helper libraries specific to Debian users.

;; This package contains the following libraries:
;;
;; `apt-sources' Major-mode for editing Debian sources.list files.
;; `apt-utils'   Interface to APT (Debian package management).
;; `debian-bug'  An Emacs command to submit a bug report.
;; `deb-view'    View contents of Debian package, similarly to tar-mode.
;; `gnus-BTS'    Provides buttons for bug numbers seen in Gnus messages.
;; `preseed'     Major-mode for editing debian-installer preseed files.

;;; History:
;;
;; 2008-04-12 - Géraud Meyer
;;  - Use apt-sources-mode for files in /etc/apt/sources.list.d/ too.
;;  - Use \' instead of $ for the end of filenames.
;; 2003-09-01 - Peter Galbraith
;;  - Created.

;;; Code:

(defgroup debian-el nil
  "Debian debian-el package customization."
  :group 'convenience)

;;(require 'debian-el-custom)

;; apt-sources
(add-to-list 'auto-mode-alist '("sources\\.list\\'" . apt-sources-mode))
(add-to-list 'auto-mode-alist '("sources\\.list\\.d/.*\\.list\\'" . apt-sources-mode))
(defgroup apt-sources nil "Mode for editing apt sources.list files"
  :group 'tools
  :prefix "apt-sources-"
  :link '(custom-manual "(debian-el)apt-sources")
  :load 'apt-sources
  ;;:require 'apt-sources
  :group 'debian-el)

;; apt-utils
(defgroup apt-utils nil
  "Emacs interface to APT (Debian package management)"
  :group 'tools
  :link '(url-link "http://www.tc.bham.ac.uk/~matt/AptUtilsEl.html")
  :link '(custom-manual "(debian-el)apt-utils")
  :load 'apt-utils
  ;;:require 'apt-utils
  :group 'debian-el)

;; debian-bug.el
(defgroup debian-bug nil "Debian Bug report helper"
  :group 'tools
  :prefix "debian-bug-"
  :link '(custom-manual "(debian-el)debian-bug")
  :load 'debian-bug
  ;;:require 'debian-bug
  :group 'debian-el)

;; deb-view.el
(setq auto-mode-alist
      (append '(("\\.u?deb\\'" . deb-view-mode)) auto-mode-alist))
(defgroup deb-view nil
  "View Debian package files with tar-mode"
  :group 'tools
  :prefix "deb-view"
  :link '(custom-manual "(debian-el)deb-view")
  :load 'deb-view
  :group 'debian-el)
(eval-and-compile (require 'dired))
(define-key dired-mode-map "\C-d" 'deb-view-dired-view)

(when (member 'utf-8 (coding-system-list))
  ;; The following from Kevin Ryde <user42@zip.com.au>
  ;; Closes: #484027
  (defun deb-view-control-coding (arg-list)
    "Return coding system for the \"control\" file in a deb.
This function is for use from `file-coding-system-alist'.

ARG-LIST is arguments passed to `find-operation-coding-system'.
The only operation handled here is `insert-file-contents' with a
buffer filename \".deb-INFO!./control\", for which the return is
\\='utf-8, and for anything else the return is nil (letting
`find-operation-coding-system' try other things).

This is done as a function because the filename passed to
find-operation-coding-system by tar-mode is merely the archive
member \"./control\".  By looking at the buffer-file-name we can
tell if it's from a deb.

Note: This only works in emacs22, in emacs21 or xemacs21 tar-mode
does something a bit different and doesn't reach here (and
there's no buffer passed to coding system functions)."
    (if (and (eq (car arg-list) 'insert-file-contents) ;; first arg
             (consp (cadr arg-list)) ;; second arg like ("./control" . BUFFER)
             (let ((buffer (cdr (cadr arg-list))))
               (and (buffer-file-name buffer)
                    (string-match "\\.deb-INFO!\\./control\\'"
                                  (buffer-file-name buffer))
                    'utf-8)))
        'undecided))

  (add-to-list 'file-coding-system-alist
               '("\\'control\\'" . deb-view-control-coding)))

(provide 'debian-el)

;;; debian-el.el ends here
