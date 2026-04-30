;;; dotemacs-org.el  -*- lexical-binding: t; -*-

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
  (setopt valign-fancy-bar t)
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

(defun +org-has-todo-p ()
  "Return non-nil if the current buffer contains any active TODO headlines."
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (h) (eq (org-element-property :todo-type h) 'todo))
    nil 'first-match))

(defvar org-directory)
;; TODO: handle file rename and deletion.
(defun +org-update-agenda ()
  "Automatically update the org agenda file list for the current buffer. "
  (when (and (buffer-file-name) (derived-mode-p 'org-mode))
    (if (and (file-in-directory-p (buffer-file-name) org-directory)
             (+org-has-todo-p))
        (org-agenda-file-to-front)
      (when (org-agenda-file-p)
        (org-remove-file)))))

(defun +org-setup-auto-agenda ()
  (add-hook 'after-save-hook #'+org-update-agenda nil t))

(add-hook 'org-mode-hook #'+org-setup-auto-agenda)

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

(provide 'dotemacs-org)
;;; dotemacs-org.el ends here
