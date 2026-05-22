;;; dotemacs-denote.el  -*- lexical-binding: t; -*-

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


(eval-when-compile (require 'dotemacs-core))

;;;; Denote

;; Core paths and file naming
(setopt denote-directory (expand-file-name "~/org/notes/"))
;; Known keywoards
(setopt denote-known-keywords '("blog" "emacs" "linux" "lisp" "life" "work" "coding" "tutorial" "review"))
;; Automatically rename denote buffers
(setopt denote-rename-buffer-mode t)
;; Since I will add new denote file types to the beginning of the list,
;; org will no longer be the default.
(setopt denote-file-type 'org)

;; Denote key bindings
(define-keymap :keymap mode-specific-map
  "n n"    #'denote
  "n r"    #'denote-rename-file
  "n R"    #'denote-rename-file-using-front-matter
  "n l"    #'denote-link
  "n L"    #'denote-add-links
  "n b"    #'denote-backlinks
  "n q c"  #'denote-query-contents-link
  "n q f"  #'denote-query-filenames-link
  "n d"    #'denote-dired
  ;; consult-denote
  "n g"    #'consult-denote-grep
  "n f"    #'consult-denote-find
  ;; denote-menu
  "n m"    #'denote-menu-list-notes
  ;; denote-journal
  "n j n"  #'denote-journal-new-entry
  "n j j"  #'denote-journal-new-or-existing-entry
  "n j l"  #'denote-journal-link-or-create-entry
  ;; denote-explore
  ;; Statistics
  "n c n"  #'denote-explore-count-notes
  "n c k"  #'denote-explore-count-keywords
  "n c e"  #'denote-explore-barchart-filetypes
  "n c w"  #'denote-explore-barchart-keywords
  "n c t"  #'denote-explore-barchart-timeline
  ;; random walk
  "n w n"  #'denote-explore-random-note
  "n w r"  #'denote-explore-random-regex
  "n w l"  #'denote-explore-random-link
  "n w k"  #'denote-explore-random-keyword
  ;; denote janitor
  "n j d"  #'denote-explore-duplicate-notes
  "n j D"  #'denote-explore-duplicate-notes-dired
  "n j m"  #'denote-explore-missing-links
  "n j z"  #'denote-explore-zero-keywords
  "n j s"  #'denote-explore-single-keywords
  "n j r"  #'denote-explore-rename-keyword
  "n j y"  #'denote-explore-sync-metadata
  "n j i"  #'denote-explore-isolated-files
  ;; visualise denote
  "n x n"  #'denote-explore-network
  "n x r"  #'denote-explore-network-regenerate
  "n x d"  #'denote-explore-barchart-degree
  "n x b"  #'denote-explore-barchart-backlinks
  )

(after-load! (:and denote consult)
  (consult-denote-mode))

(after-load! consult-denote
  (when (executable-find "fd")
    (setopt consult-denote-find-command #'consult-fd))
  (cond ((executable-find "git")
         (setopt consult-denote-grep-command #'consult-git-grep))
        ((executable-find "rg")
         (setopt consult-denote-grep-command #'consult-ripgrep))
        ((executable-find "ug")
         (setopt consult-denote-grep-command #'consult-ugrep))))

(after-load! denote
  ;; Add encrypted variants of all configured denote file types to `denote-file-types'
  (dolist (type (copy-sequence denote-file-types))
    (let* ((name (car type))
           (props (cdr type))
           (name-string (symbol-name name)))
      (unless (string-suffix-p "-gpg" name-string)
        (let* ((new-name (intern (concat name-string "-gpg"))))
          (unless (assq new-name denote-file-types)
            (let* ((extension (plist-get props :extension))
                   (new-extension (concat extension ".gpg"))
                   (new-props (plist-put (copy-sequence props)
                                         :extension new-extension)))
              (push (cons new-name new-props) denote-file-types))))))))

(after-load! dired
  (define-keymap :keymap dired-mode-map
    "C-c C-d C-i" #'denote-dired-link-marked-notes
    "C-c C-d C-r" #'denote-dired-rename-files
    "C-c C-d C-k" #'denote-dired-rename-marked-files-with-keywords
    "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter))

;; Integrations
(add-hook 'text-mode-hook #'denote-fontify-links-mode)
(add-hook 'dired-mode-hook #'denote-dired-mode)
(add-hook 'calendar-mode-hook #'denote-journal-calendar-mode)

(after-load! org-capture
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(provide 'dotemacs-denote)
;;; dotemacs-denote.el ends here
