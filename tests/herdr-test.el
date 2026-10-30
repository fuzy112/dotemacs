;;; herdr-test.el --- Tests for herdr.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'seq)

(defconst herdr-test--root
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory of the Emacs configuration under test.")

(add-to-list 'load-path (expand-file-name "user-lisp" herdr-test--root))
(require 'herdr)
(require 'herdr-opencode)

(defconst herdr-test--empty-agent-list
  (json-encode '((result . ((agents . []))))))

(defconst herdr-test--started-agent
  (json-encode
   '((result . ((agents . [((cwd . "/tmp/project")
                            (pane_id . "w9:p1")
                            (agent . "opencode")
                            (agent_status . "idle"))]))))))

(defun herdr-test--wait (process)
  "Wait for PROCESS and give its sentinel a chance to run."
  (while (process-live-p process)
    (accept-process-output nil 0.01))
  (accept-process-output nil 0.02))

(ert-deftest herdr-call-returns-output ()
  (let (received)
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest args)
                 (setq received args)
                 (insert "output")
                 0)))
      (should (equal (herdr--call "Context" "agent" "list") "output"))
      (should (equal received '("herdr" nil t nil "agent" "list"))))))

(ert-deftest herdr-call-reports-command-errors ()
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _)
               (insert "command failed")
               1)))
    (let ((err (should-error (herdr--call "Context" "agent" "list"))))
      (should (string-match-p "Context: command failed" (cadr err))))))

(ert-deftest herdr-call-json-parses-response ()
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _)
               (insert (json-encode
                        '((result . ((agents . [((pane_id . "p1"))]))))))
               0)))
    (let ((response (herdr--call-json "Context" "agent" "list")))
      (should (equal (plist-get (car (plist-get (plist-get response :result)
						:agents))
				:pane_id)
                     "p1")))))

(ert-deftest herdr-project-root-normalizes-path ()
  (cl-letf (((symbol-function 'project-current)
             (lambda (&rest _) '(transient . "/tmp/project/")))
            ((symbol-function 'project-root)
             (lambda (&rest _) "/tmp/project/")))
    (should (equal (herdr--project-root) "/tmp/project"))))

(ert-deftest herdr-starts-agent-in-new-workspace ()
  (let (started calls confirmation)
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(transient . "/tmp/project/")))
              ((symbol-function 'project-root)
               (lambda (&rest _) "/tmp/project/"))
              ((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (setq confirmation prompt)
                 t))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection &optional _predicate _require-match
                                _initial-input _hist default &rest _)
                 default))
              ((symbol-function 'process-file)
               (lambda (_program _infile _buffer _display &rest command)
                 (push command calls)
                 (pcase command
                   ('("agent" "list")
                    (insert (if started herdr-test--started-agent
                              herdr-test--empty-agent-list))
                    0)
                   ((pred (lambda (command)
                            (equal (seq-take command 2)
                                   '("workspace" "create"))))
                    (insert (json-encode
                             '((result . ((root_pane . ((pane_id . "w9:p1"))))))))
                    0)
                   ((pred (lambda (command)
                            (equal (seq-take command 2) '("agent" "start"))))
                    (setq started t)
                    0)
                   (_ (error "Unexpected command: %S" command))))))
      (let ((agent (herdr-get-project-agent)))
        (should (equal (plist-get agent :pane_id) "w9:p1"))
        (should (string-match-p "new workspace" confirmation))
        (should (equal (reverse calls)
                       '(("agent" "list")
                         ("workspace" "create" "--cwd" "/tmp/project"
                          "--label" "project" "--no-focus")
                         ("agent" "start" "agent-w9-p1" "--kind" "opencode"
                          "--pane" "w9:p1")
                         ("agent" "list"))))))))

(ert-deftest herdr-open-new-agent-focuses-workspace ()
  (let (received)
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(transient . "/tmp/project/")))
              ((symbol-function 'project-root)
               (lambda (&rest _) "/tmp/project/"))
              ((symbol-function 'herdr-start-project-agent)
               (lambda (root &optional FOCUS)
                 (setq received (list root FOCUS))
                 '(:pane_id "w9:p1"))))
      (should (commandp #'herdr-open-new-agent))
      (should (equal (call-interactively #'herdr-open-new-agent)
                     '(:pane_id "w9:p1")))
      (should (equal received '("/tmp/project" FOCUS))))))

(ert-deftest herdr-prompt-size-check-rejects-oversized-input ()
  (let ((herdr-prompt-max-bytes 10))
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (&rest _) (error "Must not start a process"))))
      (let ((err (should-error
                  (herdr-ask '(:pane_id "p1") "中中中中")
                  :type 'user-error)))
        (should (string-match-p "12 bytes" (cadr err)))))))

(ert-deftest herdr-prompt-size-check-allows-unlimited-size ()
  (let ((herdr-prompt-max-bytes nil)
        (herdr-focus-after-prompt nil)
        received)
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (&rest _)
                 (setq received t)
                 (make-process :name "herdr-size-test" :command '("true")
                               :noquery t)))
              ((symbol-function 'herdr--show-agent)
               (lambda (&rest _) nil)))
      (let ((process (herdr-ask '(:pane_id "p1")
				(make-string 20000 ?x))))
        (should received)
        (herdr-test--wait process)))))

(ert-deftest herdr-ask-and-send-commands-exist ()
  (dolist (command '(herdr-ask herdr-ask-region herdr-ask-dwim
		     herdr-send-region herdr-send-dwim herdr-send-buffer))
    (should (commandp command)))
  (dolist (old-command '(herdr-ask-project-agent herdr-send-prompt))
    (should-not (fboundp old-command))))

(ert-deftest herdr-read-prompt-disables-input-method-by-default ()
  (let ((herdr-prompt-input-method nil)
        (current-input-method "existing-method")
        received-args received-method)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest args)
                 (setq received-args args
                       received-method current-input-method)
                 "prompt")))
      (should (equal (herdr--read-prompt) "prompt")))
    (should (equal received-args
                   (list "Prompt: " nil 'herdr-prompt-history
                         nil 'INHERIT-INPUT-METHOD)))
    (should-not received-method)
    (should (equal current-input-method "existing-method"))))

(ert-deftest herdr-read-prompt-uses-configured-input-method ()
  (let ((herdr-prompt-input-method "chinese-py")
        (current-input-method "existing-method")
        received-args received-method)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest args)
                 (setq received-args args
                       received-method current-input-method)
                 "prompt")))
      (should (equal (herdr--read-prompt) "prompt")))
    (should (equal received-args
                   (list "Prompt: " nil 'herdr-prompt-history
                         nil 'INHERIT-INPUT-METHOD)))
    (should (equal received-method "chinese-py"))
    (should (equal current-input-method "existing-method"))))

(ert-deftest herdr-prompt-input-method-widget-offers-choices ()
  (let ((type (prin1-to-string
               (get 'herdr-prompt-input-method 'custom-type))))
    (should (string-match-p "No input method" type))
    (should (string-match-p "Specific input method" type))
    (should (string-match-p "latin-1-prefix" type))))

(ert-deftest herdr-region-text-sends-full-text-at-or-below-limit ()
  (with-temp-buffer
    (insert "0123456789")
    (let ((herdr-region-max-length 10))
      (should (equal (herdr--region-text 1 11) "0123456789")))
    (let ((herdr-region-max-length nil))
      (should (equal (herdr--region-text 1 11) "0123456789")))))

(ert-deftest herdr-region-text-summarizes-long-region ()
  (with-temp-buffer
    (insert "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMN")
    (let* ((herdr-region-max-length 10)
           (text (herdr--region-text 1 41)))
      (should (string-match-p "Full region omitted: 40 characters" text))
      (should (string-match-p (regexp-quote "First 10 characters:\nabcdefghij")
                              text))
      (should (string-match-p "20 characters omitted" text))
      (should (string-match-p (regexp-quote "Last 10 characters:\nEFGHIJKLMN")
                              text))
      (should-not (equal text "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMN")))))

(ert-deftest herdr-region-text-summarizes-to-first-and-last-two-lines ()
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\n")
    (let* ((herdr-region-max-length 10)
           (text (herdr--region-text (point-min) (point-max))))
      (should (string-match-p "First two lines:\nline1\nline2" text))
      (should (string-match-p "4 lines omitted" text))
      (should (string-match-p "Last two lines:\nline7\nline8" text))
      (should-not (string-match-p "line4" text)))))

(defconst herdr-test--emacsclient-hint
  "\n\nRead the full buffer with:\n  emacsclient --eval '(with-current-buffer \"%s\" (buffer-string))'"
  "Expected emacsclient hint for a local non-file buffer.")

(ert-deftest herdr-ask-region-full-overrides-summary ()
  (with-temp-buffer
    (insert "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMN")
    (let ((herdr-region-max-length 10)
          (name (buffer-name))
          received)
      (cl-letf (((symbol-function 'herdr-ask)
                 (lambda (_agent prompt) (setq received prompt))))
        (herdr-ask-region 'AGENT 1 41 nil 'FULL))
      (should (equal received
                     (format
                      (concat "The user selected the following text:\n\nEmacs buffer \"%s\":1:1-1:41:\n\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMN"
                              herdr-test--emacsclient-hint)
                      name name))))))

(ert-deftest herdr-ask-region-uses-file-range-for-long-file-region ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/long-region.txt")
    (insert (make-string 300 ?x))
    (let ((herdr-region-max-length 50)
          (herdr-region-preview-width 40)
          received)
      (cl-letf (((symbol-function 'herdr-ask)
                 (lambda (_agent prompt)
                   (setq received prompt))))
        (herdr-ask-region 'AGENT 1 301))
      (should (string-match-p
               "Read /tmp/long-region.txt lines 1-1" received))
      (should (string-match-p "1 line" received))
      (should (string-match-p "…" received))
      (should-not (string-match-p (make-string 300 ?x) received)))))

(ert-deftest herdr-region-text-file-region-points-at-file-lines ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/herdr-region.txt")
    (insert (mapconcat (lambda (number) (format "line%d" number))
                       (number-sequence 1 20) "\n"))
    (let* ((herdr-region-max-length 20)
           (herdr-region-preview-lines 2)
           (herdr-region-preview-width 80)
           (text (herdr--region-text (point-min) (point-max))))
      (should (string-match-p
               "Read /tmp/herdr-region.txt lines 1-20" text))
      (should (string-match-p "^    1  line1$" text))
      (should (string-match-p "^    2  line2$" text))
      (should (string-match-p "^   19  line19$" text))
      (should (string-match-p "^   20  line20$" text))
      (should (string-match-p "16 lines omitted" text))
      (should-not (string-match-p "line10" text)))))

(ert-deftest herdr-project-file-name-uses-project-relative-path ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/project/src/file.el")
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(transient . "/tmp/project/")))
              ((symbol-function 'project-root)
               (lambda (&rest _) "/tmp/project/")))
      (should (equal (herdr--project-file-name) "src/file.el")))))

(ert-deftest herdr-ask-region-sends-project-relative-file-name ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/project/src/file.el")
    (insert "selected text")
    (let (received)
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) '(transient . "/tmp/project/")))
                ((symbol-function 'project-root)
                 (lambda (&rest _) "/tmp/project/"))
                ((symbol-function 'herdr-ask)
                 (lambda (_agent prompt) (setq received prompt))))
        (herdr-ask-region 'AGENT 1 14 "Review this"))
      (should (equal received
                     (concat "Review this\n\n---\n"
                             "The user selected the following text:\n\nsrc/file.el:1:1-1:14:\n\nselected text"))))))

(ert-deftest herdr-ask-region-separates-prompt-and-selected-text ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/herdr-region.txt")
    (insert "selected text")
    (let ((herdr-region-max-length 1000)
          received)
      (cl-letf (((symbol-function 'herdr-ask)
                 (lambda (_agent prompt)
                   (setq received prompt)
                   'PROCESS)))
        (should (eq (herdr-ask-region 'AGENT 1 14 "Review this")
                    'PROCESS)))
      (should (equal received
                     (concat "Review this\n\n---\n"
                             "The user selected the following text:\n\n/tmp/herdr-region.txt:1:1-1:14:\n\n"
                             "selected text"))))))

(ert-deftest herdr-ask-region-non-file-buffer-uses-buffer-name ()
  (with-temp-buffer
    (insert "selected text")
    (let ((name (buffer-name))
          received)
      (cl-letf (((symbol-function 'herdr-ask)
                 (lambda (_agent prompt)
                   (setq received prompt))))
        (herdr-ask-region 'AGENT 1 14 "Review this"))
      (should (equal received
                     (format
                      (concat "Review this\n\n---\nThe user selected the following text:\n\nEmacs buffer \"%s\":1:1-1:14:\n\nselected text"
                              herdr-test--emacsclient-hint)
                      name name))))))

(ert-deftest herdr-ask-dwim-allows-non-file-region ()
  (with-temp-buffer
    (insert "selected text")
    (set-mark 1)
    (goto-char 14)
    (setq mark-active t)
    (let ((transient-mark-mode t)
          (name (buffer-name))
          received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "Review this"))
                ((symbol-function 'herdr-ask)
                 (lambda (_agent prompt)
                   (setq received prompt))))
        (call-interactively #'herdr-ask-dwim))
      (should (equal received
                     (format
                      (concat "Review this\n\n---\nThe user selected the following text:\n\nEmacs buffer \"%s\":1:1-1:14:\n\nselected text"
                              herdr-test--emacsclient-hint)
                      name name))))))

(ert-deftest herdr-ask-dwim-markers-track-prompt-edits ()
  (with-temp-buffer
    (insert "XXselected text")
    (set-mark 3)
    (goto-char 16)
    (setq mark-active t)
    (let ((transient-mark-mode t)
          (name (buffer-name))
          received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'herdr--read-prompt)
                 (lambda (&rest _)
                   (delete-region (point-min) 3)
                   "Review"))
                ((symbol-function 'herdr-ask)
                 (lambda (_agent prompt) (setq received prompt))))
        (call-interactively #'herdr-ask-dwim))
      (should (equal received
                     (format
                      (concat "Review\n\n---\nThe user selected the following text:\n\nEmacs buffer \"%s\":1:1-1:14:\n\nselected text"
                              herdr-test--emacsclient-hint)
                      name name))))))

(ert-deftest herdr-agent-remote-detects-tramp-directory ()
  (with-temp-buffer
    (let ((default-directory "/ssh:host:/tmp/"))
      (should (equal (herdr--agent-remote) "/ssh:host:"))))
  (with-temp-buffer
    (should-not (herdr--agent-remote))))

(ert-deftest herdr-buffer-read-hint-file-buffer-returns-nil ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/herdr-file.el")
    (should-not (herdr--buffer-read-hint))))

(ert-deftest herdr-buffer-read-hint-local-non-file-buffer ()
  (with-temp-buffer
    (insert "content")
    (let ((name (buffer-name)))
      (should (equal (herdr--buffer-read-hint)
                     (format herdr-test--emacsclient-hint name))))))

(ert-deftest herdr-buffer-read-hint-remote-non-file-buffer ()
  (with-temp-buffer
    (insert "content")
    (cl-letf (((symbol-function 'herdr--agent-remote)
               (lambda () "/ssh:host:"))
              ((symbol-function 'herdr--dump-buffer-remote)
               (lambda () "/tmp/herdr-buffer-abc.txt")))
      (should (equal (herdr--buffer-read-hint)
                     "\n\nThe full buffer contents are at:\n  /tmp/herdr-buffer-abc.txt")))))

(ert-deftest herdr-dump-buffer-remote-writes-and-localizes ()
  (with-temp-buffer
    (insert "remote content")
    (let (written)
      (cl-letf (((symbol-function 'herdr--agent-remote)
                 (lambda () "/ssh:host:"))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest _) "/ssh:host:/tmp/herdr-buffer-xyz.txt"))
                ((symbol-function 'write-region)
                 (lambda (beg end file &rest _)
                   (setq written (list beg end file))
                   file)))
        (should (equal (herdr--dump-buffer-remote)
                       "/tmp/herdr-buffer-xyz.txt"))
        (should (equal written
                       (list (point-min) (point-max)
                             "/ssh:host:/tmp/herdr-buffer-xyz.txt")))))))

(ert-deftest herdr-ask-dwim-prefers-active-region ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/herdr-dwim.el")
    (insert "(defun one () 1)\n(defun two () 2)\n")
    (set-mark 1)
    (goto-char 10)
    (setq mark-active t)
    (let ((transient-mark-mode t)
          received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'herdr--read-prompt)
                 (lambda (&rest _) ""))
                ((symbol-function 'herdr-ask-region)
                 (lambda (&rest args)
                   (setq received args)
                   'PROCESS)))
        (should (eq (call-interactively #'herdr-ask-dwim) 'PROCESS)))
      (should (eq (nth 0 received) 'AGENT))
      (should (markerp (nth 1 received)))
      (should (markerp (nth 2 received)))
      (should (= (marker-position (nth 1 received)) 1))
      (should (= (marker-position (nth 2 received)) 10))
      (should (equal (nthcdr 3 received) '(nil nil))))))

(ert-deftest herdr-ask-dwim-sends-defun-and-instructions ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/herdr-dwim.el")
    (insert "(defun one ()\n  (message \"one\"))\n\n(defun two () 2)\n")
    (goto-char 15)
    (setq mark-active nil)
    (let (received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "Review this"))
                ((symbol-function 'herdr-ask-region)
                 (lambda (&rest args) (setq received args))))
        (call-interactively #'herdr-ask-dwim))
      (should (eq (nth 0 received) 'AGENT))
      (should (equal (nth 3 received) "Review this"))
      (let ((text (buffer-substring-no-properties (nth 1 received)
                                                 (nth 2 received))))
        (should (string-match-p "\\`(defun one" text))
        (should-not (string-match-p "(defun two" text))))))

(ert-deftest herdr-ask-dwim-prefix-sends-full-text ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/herdr-dwim.el")
    (insert "(defun one () 1)\n")
    (goto-char 10)
    (let ((current-prefix-arg '(4))
          received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'herdr--read-prompt)
                 (lambda (&rest _) "Review this"))
                ((symbol-function 'herdr-ask-region)
                 (lambda (&rest args) (setq received args))))
        (call-interactively #'herdr-ask-dwim))
      (should (equal (nth 3 received) "Review this"))
      (should (equal (nth 4 received) '(4))))))

(ert-deftest herdr-ask-dwim-empty-instructions-is-omitted ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/herdr-dwim.el")
    (insert "(defun one () 1)\n")
    (goto-char 10)
    (let (received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'herdr--read-prompt)
                 (lambda (&rest _) ""))
                ((symbol-function 'herdr-ask-region)
                 (lambda (&rest args) (setq received args))))
        (call-interactively #'herdr-ask-dwim))
      (should (equal (nth 3 received) nil)))))

(ert-deftest herdr-send-region-sends-only-text ()
  (with-temp-buffer
    (insert "raw selected text")
    (let (received)
      (cl-letf (((symbol-function 'herdr-ask)
                 (lambda (_agent prompt) (setq received prompt))))
        (herdr-send-region 'AGENT (point-min) (point-max)))
      (should (equal received "raw selected text")))))

(ert-deftest herdr-send-dwim-sends-defun-text ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/herdr-send.el")
    (insert "(defun one ()\n  (message \"one\"))\n\n(defun two () 2)\n")
    (goto-char 15)
    (setq mark-active nil)
    (let (received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'herdr-ask)
                 (lambda (_agent prompt) (setq received prompt))))
        (call-interactively #'herdr-send-dwim))
      (should (string-match-p "\\`(defun one" received))
      (should-not (string-match-p "(defun two" received)))))

(ert-deftest herdr-send-dwim-rejects-without-region ()
  (cl-letf (((symbol-function 'herdr--region-or-defun-bounds)
             (lambda () nil)))
    (should-error (herdr-send-dwim) :type 'user-error)))

(ert-deftest herdr-send-buffer-sends-buffer-text ()
  (with-temp-buffer
    (insert "defun body\nmore")
    (let (received)
      (cl-letf (((symbol-function 'herdr-get-project-agent)
                 (lambda (&rest _) 'AGENT))
                ((symbol-function 'herdr-ask)
                 (lambda (_agent prompt) (setq received prompt))))
        (call-interactively #'herdr-send-buffer))
      (should (equal received "defun body\nmore")))))

(ert-deftest herdr-ask-dwim-falls-back-to-herdr-ask ()
  (let (received region)
    (cl-letf (((symbol-function 'herdr--region-or-defun-bounds)
               (lambda () nil))
              ((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) 'AGENT))
              ((symbol-function 'herdr--read-prompt)
               (lambda (&rest _) "Explain this"))
              ((symbol-function 'herdr-ask)
               (lambda (&rest args) (setq received args)))
              ((symbol-function 'herdr-ask-region)
               (lambda (&rest args) (setq region args))))
      (call-interactively #'herdr-ask-dwim))
    (should (equal received '(AGENT "Explain this" nil)))
    (should-not region)))

(ert-deftest herdr-final-agent-output-keeps-last-prose ()
  (let* ((raw (string-join
               '("  ┃ user question"
                 ""
                 "     + Thought · 1s"
                 ""
                 "  ┃  $ tool"
                 "  ┃  tool output"
                 ""
                 "     + Thought · 2s"
                 ""
                 "     final answer"
                 "     second line"
                 ""
                 "  ┃  # Questions"
                 "  ┃  waiting")
               "\n"))
         (cleaned (herdr--clean-agent-output
                   (or (herdr--final-agent-output raw) raw))))
    (should (default-value 'herdr-output-final-only))
    (should (equal cleaned "final answer\nsecond line"))
    (should-not (string-match-p "user question" cleaned))
    (should-not (string-match-p "tool output" cleaned))))

(ert-deftest herdr-final-agent-output-returns-nil-without-thought ()
  (should-not (herdr--final-agent-output "ordinary output")))

(ert-deftest herdr-clean-agent-output-removes-tui-chrome ()
  (let* ((output (string-join
                  (list (concat "  ┃ code" (make-string 30 ?\s) "Sidebar")
                        "  + Thought · 1s"
                        "  ┃  answer text"
                        "  ┃"
                        "╹▀▀▀▀"
                        "   ⬝⬝⬝ esc interrupt"
                        "  ┃  Build · Model")
                  "\n"))
         (cleaned (herdr--clean-agent-output output)))
    (should (string-match-p "code" cleaned))
    (should (string-match-p "answer text" cleaned))
    (dolist (junk '("Sidebar" "Thought" "Build" "esc interrupt"
                    "┃" "╹" "⬝"))
      (should-not (string-match-p (regexp-quote junk) cleaned)))))

(ert-deftest herdr-output-display-defaults-to-minibuffer ()
  (should (eq (default-value 'herdr-output-display) 'minibuffer)))

(ert-deftest herdr-show-uses-default-output-lines ()
  (let ((herdr-output-display 'per-agent-buffer)
        (herdr-output-lines 77)
        command displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'process-file)
               (lambda (&rest args)
                 (setq command args)
                 (insert "output")
                 0))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)
                 'WINDOW)))
      (herdr-show-project-agent)
      (should (equal command
                     '("herdr" nil t nil "agent" "read" "w9:p1"
                       "--source" "recent-unwrapped" "--lines" "77")))
      (kill-buffer displayed))))

(ert-deftest herdr-show-can-disable-tui-cleaning ()
  (let ((herdr-output-display 'per-agent-buffer)
        (herdr-output-final-only nil)
        (herdr-output-clean-tui nil)
        (raw "  ┃ raw UI\n\n     answer\n")
        displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'process-file)
               (lambda (&rest _)
                 (insert raw)
                 0))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)
                 'WINDOW)))
      (herdr-show-project-agent)
      (with-current-buffer displayed
        (should (equal (buffer-string) raw)))
      (kill-buffer displayed))))

(ert-deftest herdr-show-and-refresh-agent-output ()
  (let ((herdr-output-display 'per-agent-buffer)
        commands displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'process-file)
               (lambda (&rest args)
                 (push args commands)
                 (insert (if (= (length commands) 1) "recent output" "refreshed"))
                 0))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)
                 'WINDOW)))
      (herdr-show-project-agent 42)
      (should (equal (car commands)
                     '("herdr" nil t nil "agent" "read" "w9:p1"
                       "--source" "recent-unwrapped" "--lines" "42")))
      (should (buffer-live-p displayed))
      (with-current-buffer displayed
        (should (derived-mode-p 'herdr-agent-output-mode))
        (should buffer-read-only)
        (should (equal (buffer-string) "recent output"))
        (should (equal herdr--agent-output-agent '(:pane_id "w9:p1")))
        (should (= herdr--agent-output-lines 42))
        (should (eq (lookup-key herdr-agent-output-mode-map "g")
                    #'herdr-agent-output-refresh))
        (herdr-agent-output-refresh)
        (should (equal (buffer-string) "refreshed")))
      (kill-buffer displayed))))

(ert-deftest herdr-show-uses-fixed-shared-buffer ()
  (let ((herdr-output-display 'fixed-buffer)
        displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'process-file)
               (lambda (&rest _)
                 (insert "fixed output")
                 0))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)
                 'WINDOW)))
      (herdr-show-project-agent)
      (should (equal (buffer-name displayed) "*Herdr Agent*"))
      (should (equal (with-current-buffer displayed (buffer-string))
                     "fixed output"))
      (kill-buffer displayed))))

(ert-deftest herdr-show-uses-temporary-popup-window ()
  (let ((herdr-output-display 'temp-window)
        displayed action)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'process-file)
               (lambda (&rest _)
                 (insert "popup output")
                 0))
              ((symbol-function 'display-buffer)
               (lambda (buffer display-action &rest _)
                 (setq displayed buffer
                       action display-action)
                 'WINDOW)))
      (herdr-show-project-agent)
      (should (equal action
                     '((display-buffer-reuse-window
                        display-buffer-pop-up-window)
                       (window-height . 0.4))))
      (should (equal (buffer-name displayed) "*herdr-agent:w9:p1*"))
      (kill-buffer displayed))))

(ert-deftest herdr-show-agent-sets-project-root-directory ()
  (let ((herdr-output-display 'fixed-buffer)
        displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1" :cwd "/tmp")))
              ((symbol-function 'process-file)
               (lambda (&rest _) (insert "out") 0))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)
                 'WINDOW)))
      (herdr-show-project-agent)
      (should (equal (with-current-buffer displayed default-directory) "/tmp/"))
      (kill-buffer displayed))))

(ert-deftest herdr-show-uses-truncated-minibuffer-preview ()
  (let ((herdr-output-display 'minibuffer)
        shown displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'process-file)
               (lambda (&rest _)
                 (insert (make-string 600 ?x))
                 0))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (setq shown (apply #'format format args))))
              ((symbol-function 'display-buffer)
               (lambda (&rest _)
                 (setq displayed t)
                 'WINDOW)))
      (let ((buffer (herdr-show-project-agent)))
        (should-not displayed)
        (should (= (length shown) 503))
        (should (string-prefix-p "..." shown))
        (kill-buffer buffer)))))

(ert-deftest herdr-show-working-agent-uses-visible-output ()
  (let ((herdr-output-display 'per-agent-buffer)
        command displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _)
                 '(:pane_id "w9:p1" :agent_status "working")))
              ((symbol-function 'process-file)
               (lambda (&rest args)
                 (setq command args)
                 (insert "visible output")
                 0))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)
                 'WINDOW)))
      (herdr-show-project-agent 200)
      (should (equal command
                     '("herdr" nil t nil "agent" "read" "w9:p1"
                       "--source" "visible" "--lines" "200")))
      (with-current-buffer displayed
        (should (equal (buffer-string) "visible output")))
      (kill-buffer displayed))))

(ert-deftest herdr-show-agent-retries-visible-after-not-idle-error ()
  (let ((herdr-output-display 'per-agent-buffer)
        commands displayed)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _)
                 '(:pane_id "w9:p1" :agent_status "idle")))
              ((symbol-function 'process-file)
               (lambda (&rest args)
                 (push args commands)
                 (if (= (length commands) 1)
                     (progn
                       (insert "agent_not_idle")
                       1)
                   (insert "visible output")
                   0)))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _)
                 (setq displayed buffer)
                 'WINDOW)))
      (herdr-show-project-agent 200)
      (should (= (length commands) 2))
      (should (equal (nth 8 (cadr commands)) "recent-unwrapped"))
      (should (equal (nth 8 (car commands)) "visible"))
      (with-current-buffer displayed
        (should (equal (buffer-string) "visible output")))
      (kill-buffer displayed))))

(ert-deftest herdr-ask-agent-displays-output-after-async-success ()
  (let ((herdr-focus-after-prompt nil)
        received shown)
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer program &rest args)
                 (setq received (cons program args))
                 (make-process :name name :buffer buffer
                               :command '("true") :noquery t)))
              ((symbol-function 'herdr--show-result)
               (lambda (agent lines directory)
                 (setq shown (list agent lines directory))
                 'WINDOW))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (let* ((default-directory "/tmp/")
             (process (herdr-ask
                       '(:pane_id "w9:p1") "do work" 77)))
        (should (processp process))
        (herdr-test--wait process)
        (should (equal received
                       '("herdr" "agent" "prompt" "w9:p1" "do work" "--wait")))
        (should (equal shown '((:pane_id "w9:p1") 77 "/tmp/")))))))

(ert-deftest herdr-ask-focuses-agent-after-prompt ()
  (let ((herdr-focus-after-prompt t)
        focused)
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer _program &rest _)
                 (make-process :name name :buffer buffer
                               :command '("true") :noquery t)))
              ((symbol-function 'herdr--focus-agent)
               (lambda (agent directory)
                 (setq focused (list agent directory))))
              ((symbol-function 'herdr--show-agent)
               (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (let* ((default-directory "/tmp/")
             (process (herdr-ask '(:pane_id "w9:p1") "do work")))
        (should (equal focused '((:pane_id "w9:p1") "/tmp/")))
        (herdr-test--wait process)))))

(ert-deftest herdr-ask-agent-reports-async-failure ()
  (let ((herdr-focus-after-prompt nil)
        messages shown)
    (cl-letf (((symbol-function 'start-file-process)
               (lambda (name buffer _program &rest _args)
                 (make-process :name name :buffer buffer
                               :command '("sh" "-c" "printf prompt_failed; exit 1")
                               :noquery t)))
              ((symbol-function 'herdr--show-agent)
               (lambda (&rest _) (setq shown t)))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (let ((process (herdr-ask '(:pane_id "w9:p1") "do work")))
        (herdr-test--wait process)
        (should-not shown)
        (should (seq-find (lambda (text)
                            (and (string-match-p "prompt_failed" text)
                                 (string-match-p "failed (1)" text)))
                          messages))))))

(ert-deftest herdr-interrupt-agent-sends-ctrl-c-after-confirmation ()
  (let (received)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) t))
              ((symbol-function 'process-file)
               (lambda (&rest args)
                 (setq received args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (call-interactively #'herdr-interrupt-project-agent)
      (should (equal received
                     '("herdr" nil t nil "agent" "send-keys" "w9:p1"
                       "ctrl+c"))))))

(ert-deftest herdr-interrupt-agent-honors-declined-confirmation ()
  (cl-letf (((symbol-function 'herdr-get-project-agent)
             (lambda (&rest _) '(:pane_id "w9:p1")))
            ((symbol-function 'y-or-n-p)
             (lambda (&rest _) nil))
            ((symbol-function 'process-file)
             (lambda (&rest _) (error "Must not send Ctrl-C"))))
    (should-not (herdr-interrupt-project-agent))))

(ert-deftest herdr-answer-sends-text-and-enter ()
  (let (calls shown final-only display)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'herdr--agent-info)
               (lambda (&rest _) '(:agent_status "blocked")))
              ((symbol-function 'herdr--show-agent)
               (lambda (&rest _)
                 (setq shown t
                       final-only herdr-output-final-only
                       display herdr-output-display)))
              ((symbol-function 'herdr--read-answer)
               (lambda (&rest _) "yes"))
              ((symbol-function 'herdr--call)
               (lambda (context &rest args)
                 (push (cons context args) calls)
                 ""))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (call-interactively #'herdr-answer-project-agent)
      (should shown)
      (should-not final-only)
      (should (eq display 'temp-window))
      (should (equal (reverse calls)
                     '(("Failed to send text to herdr pane"
                        "pane" "send-text" "w9:p1" "yes")
                       ("Failed to submit herdr answer"
                        "agent" "send-keys" "w9:p1" "enter")))))))

(ert-deftest herdr-answer-can-send-key-sequence ()
  (let ((current-prefix-arg '(4))
        calls)
    (cl-letf (((symbol-function 'herdr-get-project-agent)
               (lambda (&rest _) '(:pane_id "w9:p1")))
              ((symbol-function 'herdr--agent-info)
               (lambda (&rest _) '(:agent_status "blocked")))
              ((symbol-function 'herdr--show-agent)
               (lambda (&rest _) nil))
              ((symbol-function 'read-string)
               (lambda (&rest _) "down enter"))
              ((symbol-function 'herdr--call)
               (lambda (context &rest args)
                 (push (cons context args) calls)
                 ""))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (call-interactively #'herdr-answer-project-agent)
      (should (equal (car calls)
                     '("Failed to send keys to herdr agent"
                       "agent" "send-keys" "w9:p1" "down" "enter"))))))

(ert-deftest herdr-answer-rejects-non-blocked-agent ()
  (cl-letf (((symbol-function 'herdr-get-project-agent)
             (lambda (&rest _) '(:pane_id "w9:p1")))
            ((symbol-function 'herdr--agent-info)
             (lambda (&rest _) '(:agent_status "working")))
            ((symbol-function 'herdr--show-agent)
             (lambda (&rest _) (error "Must not show output")))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) (error "Must not send input"))))
    (should-error (herdr-answer-project-agent) :type 'user-error)))

(defun herdr-test--db-option (label description)
  "Build a DB option hash-table with LABEL and DESCRIPTION."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "label" label ht)
    (puthash "description" description ht)
    ht))

(defun herdr-test--db-question (header text options)
  "Build a DB question hash-table with HEADER, TEXT, and OPTIONS.
OPTIONS is a list of (label . description)."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "header" header ht)
    (puthash "question" text ht)
    (let ((vec (make-vector (length options) nil)))
      (cl-loop for (label . description) in options
               for index from 0
               do (aset vec index (herdr-test--db-option label description)))
      (puthash "options" vec ht))
    ht))

(defun herdr-test--single-question-form ()
  "Build a single-question generic form plist."
  (list :session-id "ses_test"
        :tool-id "tool_single"
        :questions (list
                    (list :header "测试"
                          :text "这次应选择哪个结果？"
                          :options
                          (list (list :label "测试成功（推荐）"
                                      :description "文本回答能正常提交。")
                                (list :label "按键模式成功"
                                      :description "逻辑按键能完成选择。")
                                (list :label "暂时无法回答"
                                      :description nil))))))

(defun herdr-test--multi-question-form ()
  "Build a two-question generic form plist."
  (list :session-id "ses_test"
        :tool-id "tool_multi"
        :questions (list
                    (list :header "采样 1/2"
                          :text "这是第一个问题。"
                          :options
                          (list (list :label "第一个问题选 A"
                                      :description "用于观察多问题界面。")
                                (list :label "第一个问题选 B"
                                      :description "另一个选项。")))
                    (list :header "采样 2/2"
                          :text "这是第二个问题。"
                          :options
                          (list (list :label "第二个问题选 C"
                                      :description "第二个问题的选项。")
                                (list :label "第二个问题选 D"
                                      :description "另一个选项。"))))))

(defun herdr-test--multi-select-question-form ()
  "Build a single multi-select question generic form plist."
  (list :session-id "ses_test"
        :tool-id "tool_multi_select"
        :questions (list
                    (list :header "多选"
                          :text "选所有适用的项"
                          :multiple t
                          :options
                          (list (list :label "选项 A" :description "第一个")
                                (list :label "选项 B" :description "第二个")
                                (list :label "选项 C" :description "第三个"))))))

(ert-deftest herdr-opencode-question-plist-converts-db-question ()
  (let ((plist (herdr-opencode--question-plist
                (herdr-test--db-question
                 "头部" "问题文本？"
                 '(("选项一" . "描述一") ("选项二" . nil))))))
    (should (equal (plist-get plist :header) "头部"))
    (should (equal (plist-get plist :text) "问题文本？"))
    (should-not (plist-get plist :multiple))
    (should (equal (mapcar (lambda (option) (plist-get option :label))
                           (plist-get plist :options))
                   '("选项一" "选项二")))
    (should (equal (plist-get (car (plist-get plist :options)) :description)
                   "描述一"))))

(ert-deftest herdr-opencode-question-plist-extracts-multiple ()
  (let ((ht (herdr-test--db-question "头部" "问题文本？" '(("选项" . nil)))))
    (puthash "multiple" t ht)
    (should (eq (plist-get (herdr-opencode--question-plist ht) :multiple) t))))

(ert-deftest herdr-opencode-message-tool-part-finds-question-tool ()
  (let ((msg (json-parse-string
              "{\"content\":[{\"type\":\"text\",\"text\":\"hi\"},{\"type\":\"tool\",\"name\":\"question\",\"id\":\"t1\",\"state\":{\"status\":\"running\",\"input\":{\"questions\":[]}}}]}")))
    (let ((part (herdr-opencode--message-tool-part msg "question")))
      (should part)
      (should (equal (gethash "id" part) "t1"))
      (should (equal (gethash "status" (gethash "state" part)) "running")))))

(ert-deftest herdr-opencode-message-tool-part-returns-nil-when-absent ()
  (let ((msg (json-parse-string "{\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]}")))
    (should-not (herdr-opencode--message-tool-part msg "question"))))

(ert-deftest herdr-show-question-renders-single-question ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer)))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should (derived-mode-p 'herdr-question-mode))
        (should buffer-read-only)
        (should (equal (plist-get herdr--question-data :text)
                       "这次应选择哪个结果？"))
        (should (equal herdr--question-index 0))
        (should (string-match-p "1\\. 测试成功（推荐）" (buffer-string)))
        (should (string-match-p "文本回答能正常提交。" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest herdr-show-question-sets-project-root-directory ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer)))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (should (equal (with-current-buffer buffer default-directory) "/tmp/"))
      (kill-buffer buffer))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p2" :cwd "/tmp"))))
      (should (equal (with-current-buffer buffer default-directory) "/tmp/"))
      (kill-buffer buffer))))

(ert-deftest herdr-show-question-binds-number-keys ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer)))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (dolist (key '("1" "2" "3" "9"))
          (should (eq (lookup-key herdr-question-mode-map key)
                      #'herdr-question-select)))
        (should (eq (lookup-key herdr-question-mode-map (kbd "q"))
                    #'quit-window))
        (should (eq (lookup-key herdr-question-mode-map (kbd "g"))
                    #'revert-buffer))
        (should (eq revert-buffer-function #'herdr-question-refresh))
        (should (eq (lookup-key herdr-question-mode-map (kbd "RET"))
                    #'herdr-question-submit)))
      (kill-buffer buffer))))

(ert-deftest herdr-question-reuses-one-popup-window ()
  (let ((buffer (get-buffer-create "*herdr-test-question*"))
        (herdr--question-window 'fake-window)
        displayed)
    (cl-letf (((symbol-function 'window-live-p)
               (lambda (window) (eq window 'fake-window)))
              ((symbol-function 'window-buffer)
               (lambda (&rest _) buffer))
              ((symbol-function 'select-window)
               (lambda (window &rest _) window))
              ((symbol-function 'display-buffer)
               (lambda (&rest _) (setq displayed t) 'other-window)))
      (herdr--display-question buffer)
      (should-not displayed))
    (kill-buffer buffer)))

(ert-deftest herdr-question-select-single-sends-and-hides ()
  (let (sent hidden watched)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'herdr--send-keys-async)
               (lambda (pane-id keys &optional _on-error)
                 (setq sent (cons pane-id keys))))
              ((symbol-function 'herdr--hide-question-window)
               (lambda () (setq hidden t)))
              ((symbol-function 'herdr--question-watch-completion)
               (lambda (buffer) (setq watched buffer))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-select 1))
        (should (equal sent '("w9:p1" "1")))
        (should hidden)
        (should (eq watched buffer))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-select-rejects-out-of-range-option ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) (error "Must not send input"))))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should-error (herdr-question-select 5) :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest herdr-question-select-rejects-double-answer ()
  (let ((calls 0))
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'herdr--send-keys-async)
               (lambda (&rest _) (cl-incf calls)))
              ((symbol-function 'herdr--hide-question-window)
               (lambda () nil))
              ((symbol-function 'herdr--question-watch-completion)
               (lambda (&rest _) nil)))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-select 1)
          (should-error (herdr-question-select 2) :type 'user-error)
          (should (= calls 1)))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-select-advances-to-next-question ()
  (let (sent hidden)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--multi-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'herdr--call)
               (lambda (_context &rest args)
                 (setq sent args)
                 ""))
              ((symbol-function 'herdr--hide-question-window)
               (lambda () (setq hidden t))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-select 1)
          (should (equal (plist-get herdr--question-data :text)
                         "这是第二个问题。"))
          (should (equal herdr--question-index 1))
          (should-not hidden)
          (should-not herdr--question-answered)
          (should (equal sent '("agent" "send-keys" "w9:p1" "1"))))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-select-shows-review-after-last ()
  (let (hidden)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--multi-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'herdr--call)
               (lambda (&rest _) ""))
              ((symbol-function 'herdr--hide-question-window)
               (lambda () (setq hidden t))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-select 1)  ; Q1 -> Q2
          (herdr-question-select 1)  ; Q2 -> review
          (should (plist-get herdr--question-data :review))
          (should-not hidden)
          (should-not herdr--question-answered)
          (should (equal (plist-get herdr--question-data :items)
                         '(("采样 1/2" . "第一个问题选 A")
                           ("采样 2/2" . "第二个问题选 C")))))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-submit-sends-enter-on-review ()
  (let (sent hidden watched)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--multi-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'herdr--call)
               (lambda (&rest _) ""))
              ((symbol-function 'herdr--send-keys-async)
               (lambda (pane-id keys &optional _on-error)
                 (setq sent (cons pane-id keys))))
              ((symbol-function 'herdr--hide-question-window)
               (lambda () (setq hidden t)))
              ((symbol-function 'herdr--question-watch-completion)
               (lambda (buffer) (setq watched buffer))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-select 1)
          (herdr-question-select 1)   ; now on review page
          (herdr-question-submit))
        (should (equal sent '("w9:p1" "enter")))
        (should hidden)
        (should (eq watched buffer))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-submit-rejects-before-review ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) (error "Must not send input"))))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should-error (herdr-question-submit) :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest herdr-question-select-toggles-multi-select ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--multi-select-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) "")))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (herdr-question-select 1)
        (should (equal herdr--question-selected '("选项 A")))
        (should (string-match-p "\\[x\\] 选项 A" (buffer-string)))
        (herdr-question-select 2)
        (should (equal herdr--question-selected '("选项 A" "选项 B")))
        (herdr-question-select 1)      ; toggle the first one off again
        (should (equal herdr--question-selected '("选项 B")))
        (should (string-match-p "\\[ \\] 选项 A" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest herdr-question-finalize-multiple-shows-review ()
  (let (sent)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--multi-select-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'herdr--call)
               (lambda (_context &rest args)
                 (push args sent)
                 "")))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-select 1)
          (herdr-question-select 3)
          (herdr-question-submit)    ; finalize -> review
          (should (plist-get herdr--question-data :review))
          (should (equal (plist-get herdr--question-data :items)
                         '(("多选" . "选项 A, 选项 C")))))
        (should (equal sent '(("agent" "send-keys" "w9:p1" "right")
                              ("agent" "send-keys" "w9:p1" "3")
                              ("agent" "send-keys" "w9:p1" "1"))))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-finalize-multiple-rejects-empty ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--multi-select-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) (error "Must not send input"))))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should-error (herdr-question-submit) :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest herdr-question-next-and-prev-navigate ()
  (let (sent)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--multi-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'herdr--call)
               (lambda (_context &rest args)
                 (push args sent)
                 "")))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-next)       ; Q1 -> Q2
          (should (equal herdr--question-index 1))
          (should (equal (plist-get herdr--question-data :text)
                         "这是第二个问题。"))
          (herdr-question-next)       ; Q2 -> review
          (should (plist-get herdr--question-data :review))
          (herdr-question-prev)       ; review -> Q2
          (should (equal herdr--question-index 1))
          (should-not (plist-get herdr--question-data :review))
          (herdr-question-prev)       ; Q2 -> Q1
          (should (equal herdr--question-index 0)))
        (should (equal sent '(("agent" "send-keys" "w9:p1" "left")
                              ("agent" "send-keys" "w9:p1" "left")
                              ("agent" "send-keys" "w9:p1" "right")
                              ("agent" "send-keys" "w9:p1" "right"))))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-repick-replaces-answer ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--multi-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) "")))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (herdr-question-select 1)
        (should (equal (aref herdr--question-answers 0) "第一个问题选 A"))
        (herdr-question-prev)         ; back to Q1
        (should (equal herdr--question-index 0))
        (herdr-question-select 2)     ; change the answer
        (should (equal (aref herdr--question-answers 0) "第一个问题选 B")))
      (kill-buffer buffer))))

(ert-deftest herdr-question-retoggle-updates-selection ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--multi-select-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) "")))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (herdr-question-select 1)
        (herdr-question-select 2)
        (should (equal herdr--question-selected '("选项 A" "选项 B")))
        (should (equal (aref herdr--question-answers 0) '("选项 A" "选项 B")))
        (herdr-question-submit)         ; finalize -> review
        (herdr-question-prev)           ; back to the question
        (should (equal herdr--question-selected '("选项 A" "选项 B")))
        (herdr-question-select 2)       ; toggle B off
        (should (equal herdr--question-selected '("选项 A")))
        (should (equal (aref herdr--question-answers 0) '("选项 A"))))
      (kill-buffer buffer))))

(ert-deftest herdr-question-answer-text-sends-text-and-hides ()
  (let (sent hidden watched)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'read-string)
               (lambda (&rest _) "自定义答案"))
              ((symbol-function 'herdr--call)
               (lambda (_context &rest args)
                 (push args sent)
                 ""))
              ((symbol-function 'herdr--hide-question-window)
               (lambda () (setq hidden t)))
              ((symbol-function 'herdr--question-watch-completion)
               (lambda (buffer) (setq watched buffer))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-answer-text))
        ;; Three options -> "Type your own answer" is the 4th.
        (should (equal sent '(("agent" "send-keys" "w9:p1" "enter")
                              ("pane" "send-text" "w9:p1" "自定义答案")
                              ("agent" "send-keys" "w9:p1" "4"))))
        (should hidden)
        (should (eq watched buffer))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-select-last-option-triggers-free-text ()
  (let (sent)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'read-string)
               (lambda (&rest _) "自定义"))
              ((symbol-function 'herdr--call)
               (lambda (_context &rest args)
                 (push args sent)
                 ""))
              ((symbol-function 'herdr--hide-question-window)
               (lambda () nil))
              ((symbol-function 'herdr--question-watch-completion)
               (lambda (&rest _) nil)))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          ;; 3 options; option 4 is the visible "Type your own answer" entry.
          (herdr-question-select 4))
        (should (equal sent '(("agent" "send-keys" "w9:p1" "enter")
                              ("pane" "send-text" "w9:p1" "自定义")
                              ("agent" "send-keys" "w9:p1" "4"))))
        (kill-buffer buffer)))))

(ert-deftest herdr-show-question-shows-free-text-entry ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer)))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should (string-match-p "4\\. Type your own answer" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest herdr-show-question-multi-select-hides-free-text-entry ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--multi-select-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer)))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should-not (string-match-p "Type your own answer" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest herdr-question-answer-text-advances-multi-question ()
  (let (sent)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--multi-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'read-string)
               (lambda (&rest _) "自由输入"))
              ((symbol-function 'herdr--call)
               (lambda (_context &rest args)
                 (push args sent)
                 "")))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (herdr-question-answer-text)
          (should (equal (plist-get herdr--question-data :text)
                         "这是第二个问题。"))
          (should (equal herdr--question-index 1)))
        ;; Two options -> "Type your own answer" is the 3rd.
        (should (equal sent '(("agent" "send-keys" "w9:p1" "enter")
                              ("pane" "send-text" "w9:p1" "自由输入")
                              ("agent" "send-keys" "w9:p1" "3"))))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-answer-text-rejects-empty ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            ((symbol-function 'read-string)
             (lambda (&rest _) ""))
            ((symbol-function 'herdr--call)
             (lambda (&rest _) (error "Must not send input"))))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should-error (herdr-question-answer-text) :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest herdr-question-refresh-closes-when-done ()
  (let (closed)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              (herdr-question-status-functions
               (list (lambda (&rest _) "completed")))
              ((symbol-function 'herdr--close-question-buffer)
               (lambda (buffer) (setq closed buffer) (kill-buffer buffer)))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (should-not (herdr-question-refresh)))
        (should (eq closed buffer))
        (should-not (buffer-live-p buffer))))))

(ert-deftest herdr-question-refresh-keeps-when-running ()
  (cl-letf ((herdr-question-fetch-functions
             (list (lambda (&rest _) (herdr-test--single-question-form))))
            ((symbol-function 'display-buffer)
             (lambda (buffer &rest _) buffer))
            (herdr-question-status-functions
             (list (lambda (&rest _) "running")))
            ((symbol-function 'herdr--close-question-buffer)
             (lambda (&rest _) (error "Must not close"))))
    (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
      (with-current-buffer buffer
        (should (herdr-question-refresh))
        (should (buffer-live-p buffer)))
      (kill-buffer buffer))))

(ert-deftest herdr-question-check-completion-closes-when-completed ()
  (let (closed)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              (herdr-question-status-functions
               (list (lambda (&rest _) "completed")))
              ((symbol-function 'herdr--close-question-buffer)
               (lambda (buffer) (setq closed buffer) (kill-buffer buffer))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (herdr--question-check-completion buffer "ses_test" "tool_single")
        (should (eq closed buffer))
        (should-not (buffer-live-p buffer))))))

(ert-deftest herdr-question-check-completion-ignores-stale-buffer ()
  (let (closed)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              (herdr-question-status-functions
               (list (lambda (&rest _) "completed")))
              ((symbol-function 'herdr--close-question-buffer)
               (lambda (buffer) (setq closed buffer))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        ;; A mismatched tool id means the buffer was reused; do nothing.
        (herdr--question-check-completion buffer "ses_test" "other_tool")
        (should-not closed)
        (should (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-check-completion-reopens-on-timeout ()
  (let (failed)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              (herdr-question-status-functions
               (list (lambda (&rest _) "running")))
              ((symbol-function 'herdr--question-answer-failed)
               (lambda (buffer) (setq failed buffer))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (setq herdr--question-pending (- (float-time) 1)))
        (herdr--question-check-completion buffer "ses_test" "tool_single")
        (should (eq failed buffer))
        (kill-buffer buffer)))))

(ert-deftest herdr-question-answer-failed-restores-popup ()
  (let (shown)
    (cl-letf ((herdr-question-fetch-functions
               (list (lambda (&rest _) (herdr-test--single-question-form))))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _) buffer))
              ((symbol-function 'message)
               (lambda (&rest _) nil))
              ((symbol-function 'herdr--display-question)
               (lambda (buffer) (setq shown buffer))))
      (let ((buffer (herdr--show-question '(:pane_id "w9:p1" :cwd "/tmp/"))))
        (with-current-buffer buffer
          (setq herdr--question-answered t
                herdr--question-retries 5)
          (herdr--question-answer-failed buffer)
          (should-not herdr--question-answered)
          (should (= herdr--question-retries 0))
          (should (eq shown buffer)))
        (kill-buffer buffer)))))

(ert-deftest herdr-show-result-prefers-question-when-blocked ()
  (let (shown)
    (cl-letf (((symbol-function 'herdr--agent-info)
               (lambda (&rest _) '(:agent_status "blocked")))
              ((symbol-function 'herdr--show-question)
               (lambda (agent) (setq shown 'question) 'buffer))
              ((symbol-function 'herdr--show-agent)
               (lambda (&rest _) (error "Must not show output"))))
      (should (eq (herdr--show-result '(:pane_id "w9:p1") 120 "/tmp/")
                  'buffer))
      (should (eq shown 'question)))))

(ert-deftest herdr-show-result-shows-output-when-not-blocked ()
  (let (shown)
    (cl-letf (((symbol-function 'herdr--agent-info)
               (lambda (&rest _) '(:agent_status "idle")))
              ((symbol-function 'herdr--show-question)
               (lambda (&rest _) (error "Must not show question")))
              ((symbol-function 'herdr--show-agent)
               (lambda (&rest _) (setq shown t) 'buffer)))
      (should (eq (herdr--show-result '(:pane_id "w9:p1") 120 "/tmp/")
                  'buffer))
      (should shown))))

(ert-deftest herdr-question-poll-shows-new-blocked-agent-once ()
  (let ((herdr--question-seen nil)
        (sequence 7)
        shown
        refreshed
        (buffer (get-buffer-create "*Herdr Question: w9:p1*")))
    (with-current-buffer buffer
      (herdr-question-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'herdr--call-json)
                   (lambda (&rest _)
                     `(:result (:agents ((:pane_id "w9:p1"
                                         :agent_status "blocked"
                                         :state_change_seq ,sequence
                                         :cwd "/tmp/"))))))
                  ((symbol-function 'herdr--show-question)
                   (lambda (agent)
                     (push (plist-get agent :pane_id) shown)
                     'buffer))
                  ((symbol-function 'herdr-question-refresh)
                   (lambda (&rest _) (setq refreshed t) t)))
          (herdr--question-poll)
          (should (= (length shown) 1))
          (with-current-buffer buffer
            (setq herdr--question-agent '(:pane_id "w9:p1")))
          (herdr--question-poll)
          (should (= (length shown) 1))
          (should refreshed)
          (setq sequence 8)
          (herdr--question-poll)
          (should (= (length shown) 2)))
      (kill-buffer buffer))))

(ert-deftest herdr-question-poll-reopens-closed-buffer ()
  (when (get-buffer "*Herdr Question: w9:p1*")
    (kill-buffer "*Herdr Question: w9:p1*"))
  (let ((herdr--question-seen '(("w9:p1" . 7)))
        (sequence 7)
        shown)
    (cl-letf (((symbol-function 'herdr--call-json)
               (lambda (&rest _)
                 `(:result (:agents ((:pane_id "w9:p1"
                                     :agent_status "blocked"
                                     :state_change_seq ,sequence
                                     :cwd "/tmp/"))))))
              ((symbol-function 'herdr--show-question)
               (lambda (agent)
                 (push (plist-get agent :pane_id) shown)
                 'buffer)))
      (herdr--question-poll)
      (should (equal shown '("w9:p1"))))))

(ert-deftest herdr-question-poll-forgets-agents-that-stop-blocking ()
  (let ((herdr--question-seen nil)
        (status "blocked")
        shown)
    (cl-letf (((symbol-function 'herdr--call-json)
               (lambda (&rest _)
                 `(:result (:agents ((:pane_id "w9:p1"
                                     :agent_status ,status
                                     :state_change_seq 7
                                     :cwd "/tmp/"))))))
              ((symbol-function 'herdr--show-question)
               (lambda (&rest _)
                 (push t shown)
                 'buffer)))
      (herdr--question-poll)
      (setq status "working")
      (herdr--question-poll)
      (setq status "blocked")
      (herdr--question-poll)
      (should (= (length shown) 2)))))

(ert-deftest herdr-question-auto-mode-starts-and-stops-timer ()
  (let ((herdr-question-poll-interval 5)
        (herdr--question-timer nil)
        timer)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (seconds repeat function)
                 (setq timer (list seconds repeat function))
                 'fake-timer))
              ((symbol-function 'timerp)
               (lambda (object) (eq object 'fake-timer)))
              ((symbol-function 'cancel-timer)
               (lambda (object)
                 (setq timer (list 'canceled object)))))
      (herdr-question-auto-mode 1)
      (should (equal timer '(0 5 herdr--question-poll)))
      (should (eq herdr--question-timer 'fake-timer))
      (herdr-question-auto-mode -1)
      (should (equal timer '(canceled fake-timer)))
      (should-not herdr--question-timer))))

(ert-deftest herdr-close-question-buffer-deletes-popup-window ()
  (let ((buffer (get-buffer-create "*herdr-test-close*"))
        (herdr--question-window 'fake-window)
        deleted
        killed)
    (cl-letf (((symbol-function 'window-live-p)
               (lambda (window) (eq window 'fake-window)))
              ((symbol-function 'window-buffer)
               (lambda (&rest _) buffer))
              ((symbol-function 'delete-window)
               (lambda (window) (setq deleted window)))
              ((symbol-function 'kill-buffer)
               (lambda (target) (setq killed target))))
      (herdr--close-question-buffer buffer)
      (should (eq deleted 'fake-window))
      (should (eq killed buffer))
      (should-not herdr--question-window))
    (kill-buffer buffer)))

(dolist (function '(herdr-show-project-agent herdr-ask
                    herdr-answer-project-agent
                    herdr-interrupt-project-agent))
  (unless (and (commandp function) (documentation function))
    (error "Command is not fully defined: %S" function)))

(ert-run-tests-batch-and-exit)

;;; herdr-test.el ends here
