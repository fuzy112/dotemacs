;;; dotemacs-nix.el  -*- lexical-binding: t; -*-

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

;;;; nix-mode

(declare-function nix-mode "nix-mode")

(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

(defun nix-repl-setup ()
  (setopt-local comint-indirect-setup-function #'nix-mode)
  (comint-fontify-input-mode)

  (setopt-local indent-line-function #'comint-indent-input-line-default)
  (setopt-local indent-region-function #'comint-indent-input-region-default))

(add-hook 'nix-repl-mode-hook #'nix-repl-setup)

(declare-function nix--make-repl-in-buffer "nix-repl.el")
(declare-function nix-repl-mode "nix-repl.el")

(defun nix-get-repl ()
  "Get or create the Nix REPL process.
Returns the process object for the *Nix-REPL* buffer."
  (with-current-buffer (get-buffer-create "*Nix-REPL*")
    (unless (comint-check-proc (current-buffer))
      (when-let* ((proj (project-current)))
        (setq default-directory (project-root proj)))
      (nix--make-repl-in-buffer (current-buffer))
      (nix-repl-mode))
    (get-buffer-process (current-buffer))))

(defvar comint-input-ring)
(defvar comint-input-ring-index)
(defun nix-send-string-to-repl (str &optional proc)
  "Send STR to the Nix REPL.
If PROC is not provided, uses the default Nix REPL process."
  (interactive
   (list (let (minibuffer-history histpos)
           (with-current-buffer (process-buffer (nix-get-repl))
             (setq minibuffer-history (ring-elements comint-input-ring))
             (setq histpos comint-input-ring-index))
           (read-string "Nix expr: " nil `(minibuffer-history . ,histpos)))
         nil)
   nix-mode)
  (unless proc
    (setq proc (nix-get-repl)))
  (comint-send-string proc str)
  (comint-send-string proc "\n"))

(defun nix-send-region-to-repl (start end &optional proc)
  "Send the region from START to END to the Nix REPL.
If PROC is not provided, uses the default Nix REPL process."
  (interactive (list (use-region-beginning)
                     (use-region-end)
                     nil)
               nix-mode)
  (nix-send-string-to-repl (buffer-substring start end) proc))

(defun nix-send-line-to-repl (&optional proc)
  "Send the current line to the Nix REPL.
If PROC is provided, send input to that process instead of using comint."
  (interactive nil nix-mode)
  (nix-send-region-to-repl (line-beginning-position) (line-end-position) proc))

(defun nix-send-buffer-to-repl (&optional proc)
  "Send the entire current buffer to the Nix REPL.
If PROC is provided, send input to that process instead of using comint."
  (interactive nil nix-mode)
  (nix-send-region-to-repl (point-min) (point-max) proc))

(defun nix-load-file (&optional proc)
  "Load the current file or a selected file into the Nix REPL.
With prefix argument, prompt for a file to load.
If PROC is provided, load file to that process instead of using comint."
  (interactive nil nix-mode)
  (let ((file (if (or current-prefix-arg (not (buffer-file-name)))
                  (read-file-name "Nix file: ")
                (buffer-file-name))))
    (nix-send-string-to-repl (concat ":l " (expand-file-name file) "\n") proc)))

(defun nix-load-flake (&optional proc)
  "Load the current flake into the Nix REPL.
With prefix argument, prompt for a file to find the flake from.
If PROC is provided, load file to that process instead of using comint."
  (interactive nil nix-mode)
  (let ((flake (if current-prefix-arg
                   (read-directory-name "Starting directory: ")
                 (locate-dominating-file default-directory "flake.nix"))))
    (nix-send-string-to-repl (concat ":lf " (expand-file-name flake) "\n") proc)))

(defun nix-repl-reload (&optional proc)
  "Reload files loaded in the REPL.
If PROC is provided, use that process instead of comint."
  (interactive nil nix-mode)
  (nix-send-string-to-repl (concat ":r\n") proc))

(defun nix-switch-to-repl ()
  "Switch to the Nix REPL buffer in another window, creating it if necessary.
Returns the REPL process."
  (interactive nil nix-mode)
  (switch-to-buffer-other-window (get-buffer-create "*Nix-REPL*"))
  (unless (comint-check-proc (current-buffer))
    (nix--make-repl-in-buffer (current-buffer))
    (nix-repl-mode))
  (get-buffer-process (current-buffer)))

(after-load! nix-mode
  (define-keymap :keymap nix-mode-map
    "C-c C-l" #'nix-load-file
    "C-c C-f" #'nix-load-flake
    "C-c C-r" #'nix-repl-reload
    "C-c C-z" #'nix-switch-to-repl
    "C-c C-n" #'nix-send-region-to-repl
    "C-c C-s" #'nix-send-string-to-repl
    "C-c C-b" #'nix-send-buffer-to-repl
    "C-c C-e" #'nix-send-line-to-repl))

(after-load! nix-ts-mode
  (require 'nix-mode)                   ; for REPL
  (define-keymap :keymap nix-ts-mode-map
    "C-c C-l" #'nix-load-file
    "C-c C-f" #'nix-load-flake
    "C-c C-r" #'nix-repl-reload
    "C-c C-z" #'nix-switch-to-repl
    "C-c C-n" #'nix-send-region-to-repl
    "C-c C-s" #'nix-send-string-to-repl
    "C-c C-b" #'nix-send-buffer-to-repl
    "C-c C-e" #'nix-send-line-to-repl))

(after-load! (:and (:or nix-mode nix-ts-mode) compile)
  (alist-setq! compilation-error-regexp-alist-alist
    nix '("^ *at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'nix))


(provide 'dotemacs-nix)
;;; dotemacs-nix.el ends here
