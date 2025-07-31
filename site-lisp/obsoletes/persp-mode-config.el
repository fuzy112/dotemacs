;;; persp-mode-config.el --- Configuration for persp-mode -*- lexical-binding: t -*-
;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "30.1.50"))
;; Version: 0.1.0
;; Keywords:

;;; Commentary:
;;; Code:

(straight-use-package 'persp-mode)

(setq wg-morph-on nil)      ; switch off animation from workgrousp.el
(setq persp-autokill-buffer-on-remove 'kill-weak)
(setopt persp-add-buffer-on-after-change-major-mode t)

;; (add-hook 'window-setup-hook #'persp-mode)
(setopt persp-keymap-prefix (kbd "C-c M-p"))

;; special buffer support
(after-load! persp-mode

  (add-hook 'persp-common-buffer-filter-functions
            (lambda (buf)
              (buffer-match-p '(or (derived-mode . magit-process-mode)
                                   "\\*Compile-Log\\*"
                                   "\\*log-edit-files\\*"
                                   "\\*vc-change-log\\*")
                              buf)))

  ;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; compilation
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars '( major-mode default-directory compilation-directory
                 compilation-environment compilation-arg))

  ;; magit
  (autoload 'magit-refresh "magit-mode" nil t)
  (autoload 'magit-status-mode "magit")
  (persp-def-buffer-save/load
   :predicate (lambda (buf) (buffer-match-p '(derived-mode . magit-mode) buf))
   :tag-symbol 'def-magit-buffer
   :save-vars '( major-mode default-directory
                 magit-buffer-margin
                 magit-buffer-arguments
                 magit-buffer-diff-type
                 magit-buffer-diff-args
                 magit-buffer-diff-files
                 magit-buffer-diff-files-suspended
                 magit-buffer-file-name
                 magit-buffer-files
                 magit-buffer-log-args
                 magit-buffer-log-files
                 magit-buffer-range
                 magit-buffer-range-hashed
                 magit-buffer-refname
                 magit-buffer-revision
                 magit-buffer-revision-hash
                 magit-buffer-revisions
                 magit-buffer-typearg
                 magit-buffer-upstream)
   :mode-restore-function
   (lambda (mode)
     (require 'magit)
     (funcall mode))
   :after-load-function
   (lambda (buf &rest _)
     (with-current-buffer buf
       (run-hooks 'magit-create-buffer-hook)
       (run-hooks 'magit-setup-buffer-hook)
       (magit-refresh-buffer)
       (run-hooks 'magit-post-create-buffer-hook)))))

;; ibuffer

(defun +persp-mode--setup-ibuffer-filter-groups (&rest _)
  (interactive)
  (setq ibuffer-filter-groups
        (seq-map (lambda (persp)
                   `(,(persp-name persp)
                     (predicate . (persp-contain-buffer-p (current-buffer) ,persp))))
                 (seq-filter #'identity (persp-persps))))
  (ibuffer-update nil t))

(after-load! (:and persp-mode ibuffer)
  (advice-add #'ibuffer :after #'+persp-mode--setup-ibuffer-filter-groups))

;; consult
(after-load! (:and persp-mode consult)
  (defvar persp-mode-consult-source
    (list :name "Persp"
          :narrow ?s
          :category 'buffer
          :state #'consult--buffer-state
          :history 'buffer-name-history
          :default t
          :items
          (lambda () (consult--buffer-query :sort 'visibility
                                       :predicate (lambda (buf) (persp-contain-buffer-p buf))
                                       :as #'buffer-name))))
  (eval '(consult-customize consult--source-buffer :hidden t :default nil))
  (add-to-list 'consult-buffer-sources 'persp-mode-consult-source))

;; project

(defun +persp/project-to-persp ()
  "Create a perspective from the current project.
Buffers in the project are added to the perspective."
  (interactive)
  (let* ((project (project-current t))
         (name (project-name project)))
    (unless (persp-with-name-exists-p name)
      (persp-switch name)
      (project-dired))
    (persp-switch name)
    (dolist (buf (project-buffers project))
      (with-current-buffer buf
        (persp-after-change-major-mode-h)))))

(defun +persp/switch-project-and-persp ()
  (interactive)
  (let ((project-switch-commands '+persp/project-to-persp))
    (call-interactively 'project-switch-project)))

(defun +persp/add-project ()
  "Add buffers in the current project to the current perspective."
  (interactive)
  (let* ((project (project-current t)))
    (dolist (buf (project-buffers project))
      (with-current-buffer buf
        (persp-after-change-major-mode-h)))))

(defun +persp/remove-project ()
  "Remove buffers in the current project from the current perspective."
  (interactive)
  (let* ((project (project-current t)))
    (persp-remove-buffer (project-buffers project))))

(defun +persp/shrink-to-project ()
  "Remove buffers not in the current project from the current perspective."
  (interactive)
  (let* ((project (project-current t))
         (persp (get-current-persp)))
    (cl-assert (seq-every-p #'bufferp (persp-buffers persp)))
    (cl-assert (seq-every-p #'bufferp (project-buffers project)))
    (persp-remove-buffer
     (seq-difference (persp-buffers persp)
                     (project-buffers project)))))

(after-load! persp-mode
  (define-keymap :keymap persp-key-map
    "P" #'+persp/switch-project-and-persp
    "O" #'+persp/project-to-persp
    "A" #'+persp/add-project
    "R" #'+persp/remove-project
    "X" #'+persp/shrink-to-project))

(provide 'persp-mode-config)
;;; persp-mode-config.el ends here
