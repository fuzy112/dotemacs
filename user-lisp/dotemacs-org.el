;;; dotemacs-org.el  -*- lexical-binding: t; -*-

;; Copyright © 2024-2026  Zhengyi Fu <i@fuzy.me>

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


;;;; org

(eval-when-compile (require 'dotemacs-core))

(defvar org-hide-emphasis-markers)

(defun +org/toggle-emphasis-markers ()
  "Toggle the display of emphasis markers."
  (interactive)
  (setopt org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-flush))

(after-load! org
  (setopt org-modern-table nil)
  (add-hook 'org-mode-hook #'org-modern-mode)
  ;; (setopt valign-fancy-bar t)
  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (define-keymap :keymap org-mode-map
    "C-c o M" #'+org/toggle-emphasis-markers
    "C-c o m" #'org-modern-mode
    "M-g o"   #'consult-org-heading)

  ;; Configure org TODO state workflow:
  ;; Sequence of states with keybindings and state change hooks:
  ;; - TODO: Incomplete active task, accessible via 't' key
  ;; - WAIT: Blocked/pending task, accessible via 'w' key; automatically records timestamp and note when entering this state
  ;; - |: Separator marks transition from open to closed states
  ;; - DONE: Completed task, accessible via 'd' key
  ;; - CANCEL: Abandoned task, accessible via 'c' key; automatically records note when entering this state
  (setopt org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d)" "CANCEL(c@)")))

  ;; Set persistent agenda file registry: store list of agenda-enabled files in a dedicated
  ;; plaintext file in the org root directory, for easy manual editing and persistence across sessions
  (setopt org-agenda-files (expand-file-name ".agenda-files.txt" org-directory)))

(declare-function org-element-parse-buffer "org-element.el")
(declare-function org-element-map "org-element.el")

(after-load! ox-latex
  (alist-setq! org-latex-classes
    "ctexart" '("\\documentclass[11pt]{ctexart}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after-load! org-capture

  (add-to-list 'org-capture-templates
               `("i" "Inbox" entry (file "inbox.org")
                 "* TODO %?\n  %i\n  %a"))

  (add-to-list 'org-capture-templates
               `("m" "Meeting" entry (file+headline "agenda.org" "Meetings")
                 ,(concat "* %? :meeting:\n"
                          "<%<%Y-%m-%d %a %H:00>>")))

  (add-to-list 'org-capture-templates
               '("j" "Journal" entry (file+datetree "journal.org")
                 "* %U - %^{Heading} %^g\n%?\n")))

;;;; org-protocol

(record-time! org-protocol
  (autoload 'org--protocol-detect-protocol-server "org-protocol")
  (advice-add 'server-visit-files :around #'org--protocol-detect-protocol-server))

(defun org-protocol-clone-repo (info)
  "Clone the git repository at :url and register it in `org-protocol-project-alist'.
INFO is a property list from the org-protocol request.

To use this protocol, add the following bookmarklet to your browser:

    javascript:location.href='org-protocol://clone-repo?'+new URLSearchParams({url:location.href});void(0);
"
  (let* ((parts (org-protocol-parse-parameters info))
         (url (plist-get parts :url)))
    (unless url
      (error "org-protocol-clone-repo: missing :url parameter"))
    (setq url (org-protocol-sanitize-uri url))
    (let* ((web-url (replace-regexp-in-string "\\.git/*\\'" "" url))
           (web-url (replace-regexp-in-string "/+\\'" "" web-url))
           (clone-url (concat web-url ".git"))
           (repo-name (file-name-nondirectory web-url))
           (default-dest (expand-file-name repo-name "~/src"))
           (dest (read-directory-name
                  (format "Clone “%s” to: " repo-name)
                  (file-name-directory default-dest)
                  default-dest
                  nil)))
      (setq dest (directory-file-name dest))
      (message "Cloning %s into %s..." clone-url dest)
      (let ((exit (call-process "git" nil "*org-protocol-git-clone*" t
                                "clone" "--" clone-url dest)))
        (unless (zerop exit)
          (pop-to-buffer "*org-protocol-git-clone*")
          (error "Git clone failed (exit %d)" exit))
        (message "Cloned to %s" dest)
        (let* ((base-url (concat web-url "/"))
               (wdir (file-name-as-directory dest))
               (rewrites
                (cond
                 ((string-match-p "github\\.com" web-url)
                  (list (cons (concat "^\\(" (regexp-quote base-url)
                                      "blob/[^/]+/\\)")
                              "")))
                 ((string-match-p "gitlab\\.com" web-url)
                  (list (cons (concat "^\\(" (regexp-quote base-url)
                                      "-/blob/[^/]+/\\)")
                              "")))
                 (t nil)))
               (entry (list base-url
                            :base-url base-url
                            :working-directory wdir
                            :rewrites rewrites)))
          (setq org-protocol-project-alist
                (cons entry org-protocol-project-alist))
          (customize-save-variable 'org-protocol-project-alist
                                   org-protocol-project-alist)
          (message "Registered org-protocol project for %s" base-url)))))
  nil)

(defun org-protocol--normalize-git-url (remote-url)
  "Convert a git REMOTE-URL to its HTTPS web base URL.
Handles git@host:path, ssh://git@host/path, and https://host/path."
  (let (host path)
    (cond
     ((string-match "^git@\\([^:]+\\):\\(.+\\)$" remote-url)
      (setq host (match-string 1 remote-url)
            path (match-string 2 remote-url)))
     ((string-match "^ssh://git@\\([^/]+\\)/\\(.+\\)$" remote-url)
      (setq host (match-string 1 remote-url)
            path (match-string 2 remote-url)))
     ((string-match "^https?://\\([^/]+\\)/\\(.+\\)$" remote-url)
      (setq host (match-string 1 remote-url)
            path (match-string 2 remote-url)))
     (t (error "Unsupported remote URL format: %s" remote-url)))
    (setq path (replace-regexp-in-string "\\.git/*\\'" "" path))
    (setq path (replace-regexp-in-string "/+\\'" "" path))
    (concat "https://" host "/" path "/")))

(defun org-protocol--guess-rewrites (web-url)
  "Return :rewrites for WEB-URL based on the forge."
  (let ((host (when (string-match "^https?://\\([^/]+\\)/" web-url)
                (match-string 1 web-url))))
    (cond
     ((string= host "github.com")
      (list (cons (concat "^\\(" (regexp-quote web-url) "blob/[^/]+/\\)") "")))
     ((string= host "gitlab.com")
      (list (cons (concat "^\\(" (regexp-quote web-url) "-/blob/[^/]+/\\)") "")))
     (t nil))))

(defun org-protocol-add-repo-project (repo-root)
  "Register the git repository at REPO-ROOT in `org-protocol-project-alist'."
  (interactive (list (read-directory-name "Repository: " "~/src/" nil t)))
  (let* ((repo-root (expand-file-name repo-root))
         (git-dir (expand-file-name ".git" repo-root)))
    (unless (file-directory-p git-dir)
      (user-error "Not a git repository: %s" repo-root))
    (let* ((remote (condition-case nil
                       (car (process-lines "git" "-C" repo-root
                                           "remote" "get-url" "origin"))
                     (error (user-error "No origin remote in %s" repo-root))))
           (web-url (org-protocol--normalize-git-url remote))
           (rewrites (org-protocol--guess-rewrites web-url))
           (entry (list web-url
                        :base-url web-url
                        :working-directory (file-name-as-directory repo-root)
                        :rewrites rewrites)))
      ;; Remove old entry for the same base-url to avoid duplicates
      (setq org-protocol-project-alist
            (cl-remove web-url org-protocol-project-alist
                       :key (lambda (x) (plist-get (cdr x) :base-url))
                       :test #'string=))
      (push entry org-protocol-project-alist)
      (customize-save-variable 'org-protocol-project-alist org-protocol-project-alist)
      (message "Registered %s (%s)" repo-root web-url))))

(defun org-protocol-add-repo-projects-under (root)
  "Register git repositories that are direct children of ROOT.
Only immediate subdirectories are examined; nested directories are ignored."
  (interactive (list (read-directory-name "Parent directory: " "~/src/" nil t)))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (added 0)
         (skipped 0))
    (dolist (child (directory-files root t "\\=[^.]"))
      (when (file-directory-p child)
        (if (file-directory-p (expand-file-name ".git" child))
            (condition-case err
                (let ((remote (car (process-lines "git" "-C" child
                                                  "remote" "get-url" "origin")))
                      (web-url (org-protocol--normalize-git-url remote))
                      (rewrites (org-protocol--guess-rewrites web-url))
                      (entry (list web-url
                                   :base-url web-url
                                   :working-directory (file-name-as-directory child)
                                   :rewrites rewrites)))
                  (setq org-protocol-project-alist
                        (cl-remove web-url org-protocol-project-alist
                                   :key (lambda (x) (plist-get (cdr x) :base-url))
                                   :test #'string=))
                  (push entry org-protocol-project-alist)
                  (cl-incf added))
              (error
               (cl-incf skipped)
               (message "Skipped %s: %s" child (error-message-string err))))
          (cl-incf skipped))))
    (when (> added 0)
      (customize-save-variable 'org-protocol-project-alist org-protocol-project-alist))
    (message "Done: %d added, %d skipped" added skipped)))

(after-load! org-protocol
  (add-to-list 'org-protocol-protocol-alist
               '("clone-repo"
                 :protocol "clone-repo"
                 :function org-protocol-clone-repo
                 :kill-client t)))

(provide 'dotemacs-org)
;;; dotemacs-org.el ends here
