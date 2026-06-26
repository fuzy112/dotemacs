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
  ;; symbol  			new-path		   		old-path
  '((custom-file			"custom.el.zst" 	      nil)
    (savehist-file              	"history.eld.zst" 	      (locate-user-emacs-file "history" ".emacs-history"))
    (save-place-file            	"places.eld.zst" 	      (locate-user-emacs-file '("places.eld" "places") ".emacs-places"))
    (project-list-file			"projects.eld.zst" 	      (locate-user-emacs-file (if (>= emacs-major-version 31) '("projects.eld" "projects") "projects")))
    (recentf-save-file 		 	"recentf.eld.zst" 	      (locate-user-emacs-file '("recentf.eld" "recentf") ".recentf"))
    (project-compile-history-file  	"project-compile-history.eld.zst" (expand-file-name "project-compile-history.eld" user-emacs-directory) )
    (tramp-persistency-file-name 	"tramp.eld" 		      (locate-user-emacs-file "tramp"))
    (ecomplete-database-file     	"ecompleterc.zst" 	      (locate-user-emacs-file "ecompleterc" "~/.ecompleterc"))
    (url-configuration-directory 	"url/" 			      (locate-user-emacs-file "url/" ".url/"))
    (devdocs-data-dir	        	"devdocs/" 		      (expand-file-name "devdocs" user-emacs-directory))
    (forge-database-file 		"forge-database.sqlite"       (expand-file-name "forge-database.sqlite" user-emacs-directory))
    (undo-fu-session-directory 		"undo-fu-session/" 	      (locate-user-emacs-file "undo-fu-session" ".emacs-undo-fu-session"))
    (mastodon-client--token-file	"mastodon.plstore" 	      (concat user-emacs-directory "mastodon.plstore"))
    (eshell-directory-name 		"eshell/" 		      (locate-user-emacs-file "eshell/" ".eshell/"))
    (org-id-locations-file 		"org_id-locations.eld.zst"    (locate-user-emacs-file ".org-id-locations"))
    (bookmark-default-file		"bookmarks.eld.zst"	      (locate-user-emacs-file '("bookmarks.eld" "bookmarks") ".emacs.bmk"))
    (eww-bookmarks-directory            "/"  			      nil)
    (+eww-bookmarks-file                "eww-bookmarks"  	      (expand-file-name "eww-bookmarks" user-emacs-directory))
    (transient-history-file		"transient-history.eld.zst"   (locate-user-emacs-file "transient/history.el"))
    (bangs-cache-file			"bangs.json"		      (expand-file-name "bangs.json" user-emacs-directory))
    (ielm-history-file-name		"ielm-history.eld.zst"        (locate-user-emacs-file "ielm-history.eld"))
    (persist--directory-location	"persist"		      (locate-user-emacs-file "persist"))
    (multisession-directory             "multisession/" 	      (expand-file-name "multisession/" user-emacs-directory))
    (request-storage-directory          "request/" 		      (concat (file-name-as-directory user-emacs-directory) "request"))
    (nsm-settings-file			"network-security.eld.zst"    (locate-user-emacs-file '("network-security.eld" "network-security.data")))
    (abbrev-file-name			"abbrev_defs" 		      (locate-user-emacs-file "abbrev_defs" ".abbrev_defs"))
    (save-completions-file-name		"completions"		      (locate-user-emacs-file "completions" ".completions"))
    (ido-save-directory-list-file	"ido.last"		      (locate-user-emacs-file "ido.last" ".ido.last"))
    (mpc-data-directory			"mpc/"			      (locate-user-emacs-file "mpc" ".mpc"))
    (remember-data-file			"notes"			      (locate-user-emacs-file "notes" ".notes"))
    (shadow-info-file			"shadows"		      (locate-user-emacs-file "shadows" ".shadows"))
    (shadow-todo-file			"shadow_todo"		      (locate-user-emacs-file "shadow_todo" ".shadow_todo"))
    (calc-settings-file			"calc.el"		      (locate-user-emacs-file "calc.el" ".calc.el"))
    (diary-file				"diary"			      (locate-user-emacs-file "diary" "diary"))
    (hbmap:dir-user			"hyperbole/"		      (if (and (memq system-type '(ms-windows windows-nt ms-dos win32)) (not (getenv "HOME"))) "c:/_hyperb/" "~/.hyperb/"))
    (hbmap:dir-filename			"hyperbole/HBMAP"	      (expand-file-name "HBMAP" (if (and (memq system-type '(ms-windows windows-nt ms-dos win32)) (not (getenv "HOME"))) "c:/_hyperb/" "~/.hyperb/")))
    (gptel-gh-github-token-file		"copilot-chat/github-token"   (expand-file-name ".cache/copilot-chat/github-token" user-emacs-directory))
    (gptel-gh-token-file		"copilot-chat/token"	      (expand-file-name ".cache/copilot-chat/token" user-emacs-directory))
    (magit-user-githook-file		"magit-githooks"	      (locate-user-emacs-file "magit-githooks"))))

(defun dotemacs-state-setup ()
  "Relocate state files to `dotemacs-state-directory'.

For each entry in `dotemacs-state-file-alist', if the old path exists
and the new path does not, copy or rename the file or directory to the
new location.  Then set the default value of the symbol to the new
expanded path."
  (pcase-dolist (`(,sym ,new ,old) dotemacs-state-file-alist)
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
      (set-default sym new-path))))

(dotemacs-state-setup)

(provide 'dotemacs-state)
;;; dotemacs-state.el ends here
