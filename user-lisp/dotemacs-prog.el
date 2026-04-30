;;; dotemacs-prog.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core)
                   (require 'cond-let))

;;;; xref

(defvar +xref--max-definitions-in-buffer 5)
(defvar xref-buffer-name)

(defun +xref-window-quit ()
  (when (buffer-live-p (get-buffer xref-buffer-name))
    (quit-windows-on xref-buffer-name)))

(declare-function xref-show-definitions-buffer-at-bottom "xref.el")

(defun +xref--show-definition (fetcher alist)
  "Use `xref-show-definitions-buffer' if the candidates are few.
Otherwise use `consult-xref'.

See `xref-show-xrefs' for FETCHER and ALIST."
  (+xref-window-quit)
  (cond-let*
    [[xrefs (funcall fetcher)]]        ; retrieve candidate xrefs list
    ((length< xrefs +xref--max-definitions-in-buffer)
     ;; few results: use a small dedicated buffer
     (xref-show-definitions-buffer-at-bottom fetcher alist))
    (t                           ; many results: let consult show them
     (consult-xref fetcher alist))))

(after-load! xref
  (setopt xref-search-program
          (cond ((executable-find "rg") 'ripgrep)
                ((executable-find "ugrep") 'ugrep)
                (t 'grep))
          xref-show-definitions-function #'+xref--show-definition
          xref-auto-jump-to-first-definition t)
  (setopt xref-after-jump-hook
          (cl-nsubstitute #'reposition-window #'recenter xref-after-jump-hook))

  (global-xref-mouse-mode)
  (define-keymap :keymap xref-mouse-mode-map
    "<remap> <xref-find-definitions-at-mouse>" #'embark-dwim
    "C-M-<down-mouse-1>" #'ignore
    "C-M-<mouse-1>" #'xref-find-references-at-mouse))


;;;; display-fill-column-indicator-mode

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)


;;;; cc-mode

(after-load! cc-vars
  (setopt c-tab-always-indent nil
          c-insert-tab-function #'completion-at-point))
(defun +cc-mode--hook ()
  (add-hook 'flymake-diagnostic-functions #'flymake-clang-tidy nil t))
(add-hook 'c-mode-common-hook '+cc-mode--hook)

(alist-setq! auto-mode-alist
  "\\.[cti]pp\\'" #'c++-mode)

;;;; rust-mode

(declare-function project-prefixed-buffer-name "project.el" (arg1))
(define-advice rust--compile (:around (&rest args) project-prefix-buffer-name)
  (let ((compilation-buffer-name-function #'project-prefixed-buffer-name))
    (apply args)))


;;;; sh-script

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(defun +sh-mode-h ()
  (add-hook 'flymake-diagnostic-functions #'flymake-checkbashisms nil t))
(after-load! 'sh-script
  (add-hook 'sh-mode-hook #'+sh-mode-h))
(add-hook 'sh-mode-hook #'flymake-mode)

(defun +shellcheck-auto-fix ()
  "Run shellcheck on current buffer to generate fix suggestions.
Displays suggested fixes as a diff in a temporary buffer. Prompts
user to apply the changes interactively. Requires shellcheck
executable in PATH. Applies selected changes immediately when
confirmed."
  (interactive)
  (with-output-to-temp-buffer " *shellcheck diff*"
    (call-process-region nil nil "shellcheck" nil standard-output t "-f" "diff" "-"))
  (let ((name (or (and buffer-file-name
                       (file-name-nondirectory
                        (buffer-file-name)))
                  (buffer-name))))
    (with-current-buffer " *shellcheck diff*"
      (let ((inhibit-read-only t)
            (buffer-read-only nil))
        (replace-regexp-in-region "^\\(+++ b\\|--- a\\)/-"
                                  (concat "\\1/"
                                          (replace-quote name)))
        (diff-mode)
        (font-lock-ensure))))
  (when (or noninteractive (yes-or-no-p "Apply the patch? "))
    (with-current-buffer " *shellcheck diff*"
      (diff-apply-buffer)))
  (quit-windows-on " *shellcheck diff*" t))

;;;; verilog

(defun +verilog-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-verilator nil t))

(add-hook 'verilog-mode-hook #'+verilog-mode-setup)

;;;; javascript

(declare-function js-jsx--comment-region "js.el")

(define-advice js-jsx-enable (:after () comments)
  "Enable JSX comments."
  (setopt-local comment-region-function #'js-jsx--comment-region))

(define-advice js-jsx-enable (:after () sgml)
  "Enable sgml commands in JSX buffers."
  (eval-and-compile (require 'sgml-mode))
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map)
    (define-key map (kbd "C-c C-b") #'sgml-skip-tag-backward)
    (define-key map (kbd "C-c C-d") #'sgml-delete-tag)
    (define-key map (kbd "C-c C-e") #'sgml-close-tag)
    (define-key map (kbd "C-c C-f") #'sgml-skip-tag-forward)
    (define-key map (kbd "C-c C-o") #'sgml-tag)
    (define-key map (kbd "C-c C-t") #'sgml-tag)))

;;;; markdown

(after-load! markdown-mode
  (keymap-set markdown-mode-map      "C-x C-q" #'markdown-view-mode)
  (keymap-set markdown-view-mode-map "C-x C-q" #'markdown-mode)
  (keymap-set gfm-mode-map           "C-x C-q" #'gfm-view-mode)
  (keymap-set gfm-view-mode-map      "C-x C-q" #'gfm-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode))

;;;; pcap-mode

(alist-setq! auto-mode-alist
  "\\.pcapng\\'" #'pcap-mode)

(add-hook 'pcap-mode-hook #'hl-line-mode)

;;;; udev rules

;; Use prog-mode to edit udev rules.

(defun udev-rules-mode ()
  "Turn on `prog-mode' mode and set up for udev rules."
  (interactive)
  (prog-mode)
  (let ((st (syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)))

(alist-setq! auto-mode-alist
  "\\.rules\\'" #'udev-rules-mode)

;;;; project

;; (setq project-list-file (locate-user-emacs-file '("projects.eld.zst")))

(declare-function project-root "project.el" (&rest rest))
(defun +project--external-roots ()
  (and-let* ((project (project-current))
             (root (project-root project))
             (project-root-file (expand-file-name ".project-root" root))
             ((file-exists-p project-root-file)))
    (with-temp-buffer
      (insert-file-contents project-root-file)
      (let ((default-directory root))
        (mapcar #'expand-file-name
                (string-lines (buffer-string) t nil))))))

(after-load! project
  (setopt project-switch-commands 'project-prefix-or-any-command)
  (setq-default project-vc-external-roots-function #'+project--external-roots)
  (setopt project-compilation-buffer-name-function #'project-prefixed-buffer-name)

  (setopt project-vc-ignores (seq-union project-vc-ignores '(".pc")))
  (setopt project-vc-extra-root-markers
          (seq-union project-vc-extra-root-markers
                     '(".project-root" "configure.ac" "Cargo.toml" "package.json")))

  (project-compile-history-mode))

(defvar dotemacs-prog--project-files-ignore-vcs nil)

(cl-defmethod project-files :around (project &optional dirs)
  (let ((dotemacs-prog--project-files-ignore-vcs t))
    (cl-call-next-method)))

;; Use fd to speed up C-u C-x p f.
(define-advice project--files-in-directory (:override (dir ignores &optional files) fd)
  (require 'find-dired)
  (require 'xref)
  (let* ((dir (file-name-as-directory dir))
         (default-directory dir)
         ;; Make sure ~/ etc. in local directory name is
         ;; expanded and not left for the shell command
         ;; to interpret.
         (localdir (file-name-unquote (file-local-name (expand-file-name dir))))
         (command (format "%s --no-follow %s %s --type file --print0 %s"
                          (or (executable-find "fd")
                              (executable-find "fd-find"))
                          (if dotemacs-prog--project-files-ignore-vcs
                              "--ignore-vcs"
                            "--no-ignore-vcs")
                          (mapconcat (lambda (pat) (shell-quote-argument (concat "--exclude=" pat))) ignores " ")
                          (if files
                              (shell-quote-argument (string-join files "|"))
                            "''")))
         res)
    (with-temp-buffer
      (let ((status
             (process-file-shell-command command nil t))
            (pt (point-min)))
        (unless (zerop status)
          (goto-char (point-min))
          (if (and
               (not (eql status 127))
               (search-forward "Permission denied\n" nil t))
              (let ((end (1- (point))))
                (re-search-backward "\\`\\|\0")
                (error "File listing failed: %s"
                       (buffer-substring (1+ (point)) end)))
            (error "File listing failed: %s" (buffer-string))))
        (goto-char pt)
        (while (search-forward "\0" nil t)
          (push (buffer-substring-no-properties (+ pt 2) (1- (point)))
                res)
          (setq pt (point)))))
    (if project-files-relative-names
        (sort res #'string<)
      (project--remote-file-names
       (mapcar (lambda (s) (concat localdir s))
               (sort res #'string<))))))

(provide 'dotemacs-prog)
;;; dotemacs-prog.el ends here
