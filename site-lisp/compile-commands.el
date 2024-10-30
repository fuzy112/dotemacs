;;; compile-commands.el --- Setup flymake-cc with compilation database -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.2.0
;; Keywords: languages, c

;; This file is not part of GNU Emacs.

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

;; To use this package, add the following configuration to your
;; init.el file:
;;
;; (setf flymake-cc-command #\\'flymake-cc-use-compile-commands)
;;

;;; Code:

;;;; Requirements

(require 'json)
(require 'cl-seq)

;;;; Customization options

(defgroup compile-commands nil
  "Compilation database."
  :group 'compile-commands
  :prefix "compile-commands-")

(defcustom compile-commands-compiler nil
  "Use another syntax checker as the compiler."
  :type '(choice (const :tag "Use the original compiler" nil)
                 (string :tag "Use program")))

;;;; Internal variables

(defvar compile-commands--buffers nil)

(defvar compile-commands--cache (make-hash-table :test 'eq))

;;;; Internal functions

(defun compile-commands--make-hash (compile-commands)
  (let ((ht (make-hash-table :test 'equal)))
    (cl-loop for entry being the elements of compile-commands
             for file = (alist-get 'file entry)
             do (puthash file entry ht))
    ht))

(defun compile-commands--load ()
  (ignore-errors
    (goto-char (point-min))
    (let ((compile-commands (json-read)))
      (puthash (current-buffer)
               (compile-commands--make-hash compile-commands)
               compile-commands--cache))))

(defun compile-commands--unload ()
  (ignore-errors
    (remove-hook 'after-change-functions #'compile-commands--load t)
    (remhash (current-buffer) compile-commands--cache)
    (setf compile-commands--buffers
          (delete (current-buffer) compile-commands--buffers))))


(defun compile-commands--lookup-file (file)
  (let ((abspath (expand-file-name file)))
    (cl-loop
     for commands being the hash-values of compile-commands--cache
     for result = (gethash abspath commands)
     if result return (compile-commands--filter-arguments result))))

(defun compile-commands--filter-arguments (entry &optional output)
  (let-alist entry
    (if (not .arguments)
        (setf .arguments (split-string-shell-command .command)))
    (if (not (listp .arguments))
        (setf .arguments (append .arguments nil)))
    (when-let* ((node (cl-member "-o" .arguments :test #'equal)))
      (setf (nth 1 node) "/dev/null")
      (setf output t))
    (when-let* ((node (cl-member-if
                      (lambda (el)
                        (equal (expand-file-name el .directory)
                               .file))
                      .arguments)))
      ;; FIXME
      (setcdr node
              (nconc (list
                      "-x"
                      (if (string-suffix-p ".c" .file)
                          "c"
                        "c++")
                      (format "-I%s" (file-name-directory .file))
                      "-c"
                      "-")
                     (and output
                          (list "-o" "/dev/null"))
                     (cdr node)))
      (setcar node
              (if (string-search "clang" (car .arguments))
                  ;; For clang, `-fsyntax-only' is enough to let it perform
                  ;; syntax checking.
                  "-fsyntax-only"
                ;; GCC only performs full syntax checking when it is
                ;; actually compiling.  So let it just generate assembly.
                "-S")))

    (setf .arguments (cl-delete-if
                      (lambda (opt)
                        (or (string-prefix-p "-O" opt)
                            (string-prefix-p "-M" opt)
                            (equal opt "-fdiagnostics-color")))
                      .arguments))
    (when compile-commands-compiler
      (setf (nth 0 .arguments) compile-commands-compiler))
    `((command . ,(concat (mapconcat #'shell-quote-argument
                                     .arguments
                                     " ")
                          " 2>&1 | head -2000"))
      (directory . ,.directory)
      (file . ,.file))))

;;;; Public functions

;;;###autoload
(defun compile-commands-load (file)
  "Load compile commands from FILE.

To unload the file, just kill the buffer."
  (interactive "fCompile commands: ")
  (let ((buf (find-file-noselect file)))
    (cl-pushnew buf compile-commands--buffers)
    (with-current-buffer buf
      (add-hook 'after-change-functions #'compile-commands--load nil t)
      (add-hook 'kill-buffer-hook #'compile-commands--unload nil t)
      (compile-commands--load))))

;;;###autoload
(defun compile-commands-flymake-cc-command ()
  "Return a command for use with `flymake-cc'.

This function can be used as a value of `flymake-cc-command'."
  (and-let* ((entry (compile-commands--lookup-file (buffer-file-name)))
             (dir (alist-get 'directory entry))
             (command (alist-get 'command entry)))
    (list "/bin/sh" "-c" command)))

;;;###autoload
(defalias 'flymake-cc-use-compile-commands
  #'compile-commands-flymake-cc-command)

(provide 'compile-commands)
;;; compile-commands.el ends here
