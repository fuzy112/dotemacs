;;; dotemacs-state.el --- State files management -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

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


;;; state files

(defvar dotemacs-state-directory (concat user-emacs-directory "/var/"))
(unless (file-directory-p dotemacs-state-directory)
  (make-directory dotemacs-state-directory)
  (make-directory (concat dotemacs-state-directory "removed/")))

(defvar dotemacs-state-file-alist
  '((auto-save-list-file-prefix
     :new "auto-save-list/.saves-")
    (custom-file
     :new "custom.el.zst"
     :mode #o600)
    (savehist-file
     :new "history.eld.zst"
     :old (locate-user-emacs-file "history" ".emacs-history")
     :mode #o600)
    (save-place-file
     :new "places.eld.zst"
     :old (locate-user-emacs-file '("places.eld" "places") ".emacs-places"))
    (project-list-file
     :new "projects.eld.zst"
     :old (locate-user-emacs-file (if (>= emacs-major-version 31)
				      '("projects.eld" "projects")
				    "projects")))
    (recentf-save-file
     :new "recentf.eld.zst"
     :old (locate-user-emacs-file '("recentf.eld" "recentf") ".recentf"))
    (project-compile-history-file
     :new "project-compile-history.eld.zst"
     :old (expand-file-name "project-compile-history.eld" user-emacs-directory))
    (tramp-persistency-file-name
     :new "tramp.eld"
     :old (locate-user-emacs-file "tramp"))
    (ecomplete-database-file
     :new "ecompleterc.zst"
     :old (locate-user-emacs-file "ecompleterc" "~/.ecompleterc"))
    (url-configuration-directory
     :new "url/"
     :old (locate-user-emacs-file "url/" ".url/"))
    (devdocs-data-dir
     :new "devdocs/"
     :old (expand-file-name "devdocs" user-emacs-directory))
    (forge-database-file
     :new "forge-database.sqlite"
     :old (expand-file-name "forge-database.sqlite" user-emacs-directory)
     :mode #o600)
    (undo-fu-session-directory
     :new "undo-fu-session/"
     :old (locate-user-emacs-file "undo-fu-session" ".emacs-undo-fu-session")
     :mode #o700)
    (mastodon-client--token-file
     :new "mastodon.plstore"
     :old (concat user-emacs-directory "mastodon.plstore")
     :mode #o600)
    (eshell-directory-name
     :new "eshell/"
     :old (locate-user-emacs-file "eshell/" ".eshell/")
     :mode #o700)
    (org-id-locations-file
     :new "org_id-locations.eld.zst"
     :old (locate-user-emacs-file ".org-id-locations"))
    (bookmark-default-file
     :new "bookmarks.eld.zst"
     :old (locate-user-emacs-file '("bookmarks.eld" "bookmarks") ".emacs.bmk"))
    (eww-bookmarks-directory
     :new "/")
    (+eww-bookmarks-file
     :new "eww-bookmarks"
     :old (expand-file-name "eww-bookmarks" user-emacs-directory))
    (transient-history-file
     :new "transient-history.eld.zst"
     :old (locate-user-emacs-file "transient/history.el"))
    (bangs-cache-file
     :new "bangs.json"
     :old (expand-file-name "bangs.json" user-emacs-directory))
    (ielm-history-file-name
     :new "ielm-history.eld.zst"
     :old (locate-user-emacs-file "ielm-history.eld"))
    (persist--directory-location
     :new "persist"
     :old (locate-user-emacs-file "persist"))
    (multisession-directory
     :new "multisession/"
     :old (expand-file-name "multisession/" user-emacs-directory))
    (request-storage-directory
     :new "request/"
     :old (concat (file-name-as-directory user-emacs-directory) "request")
     :mode #o600)
    (nsm-settings-file
     :new "network-security.eld.zst"
     :old (locate-user-emacs-file '("network-security.eld" "network-security.data"))
     :mode #o600)
    (abbrev-file-name
     :new "abbrev_defs"
     :old (locate-user-emacs-file "abbrev_defs" ".abbrev_defs"))
    (save-completions-file-name
     :new "completions"
     :old (locate-user-emacs-file "completions" ".completions"))
    (ido-save-directory-list-file
     :new "ido.last"
     :old (locate-user-emacs-file "ido.last" ".ido.last"))
    (mpc-data-directory
     :new "mpc/"
     :old (locate-user-emacs-file "mpc" ".mpc"))
    (remember-data-file
     :new "notes"
     :old (locate-user-emacs-file "notes" ".notes")
     :mode #o600)
    (shadow-info-file
     :new "shadows"
     :old (locate-user-emacs-file "shadows" ".shadows"))
    (shadow-todo-file
     :new "shadow_todo"
     :old (locate-user-emacs-file "shadow_todo" ".shadow_todo"))
    (calc-settings-file
     :new "calc.el"
     :old (locate-user-emacs-file "calc.el" ".calc.el"))
    (diary-file
     :new "diary"
     :old (locate-user-emacs-file "diary" "diary")
     :mode #o600)
    (hbmap:dir-user
     :new "hyperbole/"
     :old (if (and (memq system-type '(ms-windows windows-nt ms-dos win32))
		   (not (getenv "HOME")))
	      "c:/_hyperb/"
	    "~/.hyperb/"))
    (hbmap:dir-filename
     :new "hyperbole/HBMAP"
     :old (expand-file-name "HBMAP" (if (and (memq system-type '(ms-windows windows-nt ms-dos win32))
					     (not (getenv "HOME")))
					"c:/_hyperb/"
				      "~/.hyperb/")))
    (gptel-gh-github-token-file
     :new "copilot-chat/github-token"
     :old (expand-file-name ".cache/copilot-chat/github-token" user-emacs-directory)
     :mode #o600)
    (gptel-gh-token-file
     :new "copilot-chat/token"
     :old (expand-file-name ".cache/copilot-chat/token" user-emacs-directory)
     :mode #o600)
    (magit-user-githook-file
     :new "magit-githooks"
     :old (locate-user-emacs-file "magit-githooks"))
    (xwidget-webkit-cookie-file
     :new "xwidget-webkit-cookies.txt"
     :mode #o600)))

(defun dotemacs-state-setup ()
  "Relocate state files to `dotemacs-state-directory'.

For each entry in `dotemacs-state-file-alist', if the old path exists
and the new path does not, copy or rename the file or directory to the
new location.  Then set the default value of the symbol to the new
expanded path."
  (pcase-dolist (`(,sym . ,(map :old :new :mode)) dotemacs-state-file-alist)
    (let ((new-path (abbreviate-file-name
		     (expand-file-name new dotemacs-state-directory)))
	  (old-path (eval old t)))
      (when (and old-path (not (file-exists-p new-path)) (file-exists-p old-path))
	(cond
	 ((null old))
	 ((file-directory-p old-path)
	  (rename-file old-path (directory-file-name new-path)))
	 (t
	  (make-directory (file-name-directory new-path) t)
	  (with-temp-file new-path
	    (insert-file-contents old-path))
	  (rename-file old-path
		       (concat dotemacs-state-directory
			       "removed/"
			       (file-name-nondirectory old-path))))))
      (when (and (file-exists-p new-path) mode)
	(set-file-modes new-path mode))
      (set-default sym new-path))))

(dotemacs-state-setup)

(provide 'dotemacs-state)
;;; dotemacs-state.el ends here
