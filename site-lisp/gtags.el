;;; gtags.el --- Gtags support -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.3
;; Keywords: tools

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

;; This package implements an Xref backend based on GNU Global.
;;
;; To use this package, either add `gtags-xref-backend' to
;; `xref-backend-functions' or enable `gtags-mode' or its globalized
;; version.

;;; Code:

(require 'xref)
(eval-when-compile (require 'subr-x))

(defgroup gtags ()
  "Gtags."
  :group 'tools
  :prefix "gtags-")

(defcustom gtags-global-executable "global"
  "The global executable to use."
  :type 'file)

;;;###autoload
(progn
  (defun gtags-xref-backend ()
    (and (locate-dominating-file default-directory "GTAGS")
         'gtags)))

(defun gtags-available-p ()
  (locate-dominating-file default-directory "GTAGS"))

(defun gtags--global-run (&rest args)
  (let ((dir default-directory)
        status)
    (with-current-buffer
        (get-buffer-create " *gtags*")
      (setq default-directory dir)
      (erase-buffer)
      (setq status (apply #'process-file gtags-global-executable nil t nil args))
      (when (not (zerop status))
        (goto-char (point-min))
        (user-error "global exited with %d: %s" status
                    (buffer-substring-no-properties
                     (point) (line-end-position))))
      (current-buffer))))

(defun gtags--global-lines (&rest args)
  (with-current-buffer
      (apply #'gtags--global-run args)
    (string-lines (buffer-string) t nil)))

;; `global' has different output formats, this function scans the -x
;; format output.
(defun gtags--global-scan-hits (buffer)
  ;; This regexp is copied from cedet-global.el
  (let ((re "^\\([^ ]+\\) +\\([0-9]+\\) +\\([^ ]+\\) ")
        hits)                     ; hits is a list of (LINE FILE TEXT)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (push (list (string-to-number (match-string 2))
                    (match-string 3)
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits))
      (nreverse hits))))

(cl-defmethod xref-backend-definitions ((_backend (eql 'gtags)) identifier)
  (thread-first
    (gtags--global-run "-dxa" "--literal" identifier)
    (gtags--global-scan-hits)
    (xref--convert-hits
     (concat "\\_<" (regexp-quote identifier) "\\_>"))))

(cl-defmethod xref-backend-references ((_backend (eql 'gtags)) identifier)
  (with-delayed-message (5 "ti")
    (thread-first
      (gtags--global-run "-rxa" "--literal" identifier)
      (gtags--global-scan-hits)
      (xref--convert-hits
       (concat "\\_<" (regexp-quote identifier) "\\_>")))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'gtags)))
  (let ((input 'init)
        (cands))
    (lambda (str pred action)
      (if (eq action 'metadata)
          '(metadata . ((category . identifier)))
        (when (and (not (string-empty-p str))
                   (or (eq input 'init)
                       (not (string-prefix-p input str))))
          (setq cands (gtags--global-lines "-c" str)
                input str))
        (complete-with-action action cands str pred)))))

(defun gtags--regexp-to-extended (re)
  (replace-regexp-in-string
   (regexp-quote "(?:")
   "("
   (xref--regexp-to-extended re)))

(cl-defmethod xref-backend-apropos ((_backend (eql 'gtags)) pattern)
  (let* ((re (xref-apropos-regexp pattern))
         (extended-re (gtags--regexp-to-extended re)))
    (thread-first
      (gtags--global-run "-dxa" "-e" extended-re)
      (gtags--global-scan-hits)
      (xref--convert-hits re))))

(defun gtags-completion-at-point-function ()
  (pcase-let* ((`(,beg . ,end) (bounds-of-thing-at-point 'symbol))
               (input 'init)
               (cands nil)
               (buf (current-buffer))
               (table (lambda (str pred action)
                        (unless (or (eq action 'metadata) (eq (car-safe action) 'boundaries))
                          (let ((new-input (with-current-buffer buf
                                             (buffer-substring-no-properties beg end))))
                            (when (or (eq input 'init)
                                      (not (string-prefix-p input new-input)))
                              (setq cands (gtags--global-lines "-c" new-input)
                                    input new-input)))
                          (complete-with-action action cands str pred)))))
    (list beg end table :exclusive 'no)))

(defun gtags--update-sentinel (p _m)
  (unless (process-live-p p)
    (unwind-protect
        (let (status)
          (setq status (process-exit-status p))
          (if (zerop status)
              (message "Indexing gtags...done")
            (with-current-buffer (process-buffer p)
              (goto-char (point-min))
              (message "Failed to index with status %d: %s"
                       status
                       (buffer-substring-no-properties
                        (point-min) (line-end-position))))))
      (kill-buffer (process-buffer p)))))

;;;###autoload
(defun gtags-single-update (&optional file)
  (when-let* ((file (or file (buffer-file-name)))
              (tags (locate-dominating-file file "GTAGS")))
    (with-current-buffer (generate-new-buffer "*global*")
      (message "Indexing gtags...")
      (let ((p (start-file-process "global" (current-buffer)
                                   gtags-global-executable
                                   "--single-update"
                                   (file-local-name file))))
        (set-process-sentinel p #'gtags--update-sentinel)))))

;;;###autoload
(defun gtags-update ()
  (interactive)
  (with-current-buffer (generate-new-buffer "*global*")
    (compilation-mode)
    (message "Indexing gtags...")
    (let ((p (start-file-process "global" (current-buffer)
                                 gtags-global-executable "-u")))
      (set-process-sentinel p #'gtags--update-sentinel))))

;;;###autoload
(define-minor-mode gtags-mode
  "Turn on this mode to force using Gnu Global for xref and `completion-at-point'."
  :lighter " Gtags"
  (remove-hook 'after-save-hook #'gtags-single-update t)
  (remove-hook 'xref-backend-functions #'gtags-xref-backend t)
  (remove-hook 'completion-at-point-functions #'gtags-completion-at-point-function t)
  (when gtags-mode
    (add-hook 'after-save-hook #'gtags-single-update 50 t)
    (add-hook 'xref-backend-functions #'gtags-xref-backend -50 t)
    (add-hook 'completion-at-point-functions #'gtags-completion-at-point-function -50 t)))

(defun gtags-mode--turn-on ()
  (when (and (derived-mode-p 'prog-mode)
             (or (getenv "GPATH")
                 (locate-dominating-file default-directory "GPATH")))
    (gtags-mode +1)))

;;;###autoload
(define-globalized-minor-mode gtags-global-mode
  gtags-mode
  gtags-mode--turn-on)

;;;###autoload
(defalias 'global-gtags-mode 'gtags-global-mode)

(provide 'gtags)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gtags.el ends here
