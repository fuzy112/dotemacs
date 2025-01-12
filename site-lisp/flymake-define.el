;;; flymake-define.el --- Define flymake diagnostic functions. -*- lexical-binding: t -*-
;; Copyright © 2024, 2025  Zhengyi Fu

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.8.5
;; Keywords: tools, languages

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

;; This package provides a simple way to define new flymake backends
;; that utilizes external programs.

;;; Code:

;;;; Requirements

(require 'flymake)
(require 'rx)
(eval-when-compile (require 'cl-lib))

;;;; Private rx definitions

(rx-define flymake-define-file (&rest regexps)
  (group-n 11
    (eval
     (if '(regexps)
         '(seq regexps)
       '(1+ (not (any "\n" ":")))))))
(rx-define flymake-define-line (group-n 12 (1+ digit)))
(rx-define flymake-define-column (group-n 13 (1+ digit)))
(rx-define flymake-define-message (&rest regexps)
  (group-n 14
    (eval
     (if '(regexps)
         '(seq regexps)
       '(0+ nonl)))))


;;;; Private functions

(cl-defun flymake-define--make-process-in-mntns
    ( &rest args
      &key mount-points command
      (unshare-program (executable-find "unshare"))
      &allow-other-keys)
  "Start a program in a subprocess.  Return the process object for it.

This is similar to `make-process', but accept extra arguments.

:mount-points MOUNT-POINTS -- MOUNT-POINTS is a list of (SOURCE TARGET OPTIONS)
that specifies mount-points needed for running the program.

:unshare-program UNSHARE-PROGRAM -- UNSHARE-PROGRAM is a string specifying
the name of the program used to run the command in namespaces.  If unspecified,
`unshare' is used.  See the man page `unshare(1)'.

ARGS -- see `make-process' for other arguments."
  (apply
   #'make-process
   :command
   (list unshare-program
         "--map-root-user"
         "--mount"
         "--"
         "/bin/sh"
         "-c"
         (string-join
          ;; mount-point: (source target options)
          (nconc
           (mapcar
            (pcase-lambda (`(,source ,target ,options))
              (mapconcat #'shell-quote-argument
                         `("mount" "-o" ,options ,source ,target)
                         " "))
            mount-points)
           (list (mapconcat #'shell-quote-argument command " ")))
          " && "))
   args))

(defun flymake-define--rx-rule-action (type source file)
  (let ((locus (match-string 11))
        (line (ignore-errors
                (string-to-number (match-string 12))))
        (column (ignore-errors
                  (string-to-number (match-string 13))))
        (message (match-string 14)))
    (if (null line)
        (setq line 1))
    (cond ((or (equal (and locus (expand-file-name locus)) file)
               (null locus))
           (pcase-let ((`(,beg . ,end)
                        (flymake-diag-region source line column)))
             (flymake-make-diagnostic source beg end type message)))
          (t
           (flymake-make-diagnostic locus (cons line column)
                                    nil type message)))))

(defun flymake-define--compile-rule-rx (rule)
  (let ((type (car rule))
        (regexp
         (rx-let-eval '((file (&rest regexp) (flymake-define-file regexp))
                        (line flymake-define-line)
                        (column flymake-define-column)
                        (message (&rest regexp) (flymake-define-message regexp)))
           (rx-to-string (cons 'seq (cdr rule))))))
    (cons regexp
          (apply-partially #'flymake-define--rx-rule-action type))))

(defun flymake-define--compile-rule-regexp (rule)
  (let ((re (elt rule 0))
        (act (elt rule 1)))
    (cons re act)))

(defun flymake-define--compile-rule (rule)
  (if (vectorp rule)
      (flymake-define--compile-rule-regexp rule)
    (flymake-define--compile-rule-rx rule)))

(cl-defun flymake-define--sentinel-factor
    (&key filter-color patterns debug proc-var)
  "Return a lambda, which, when invoked, creates a sentinel function.

PATTERNS is a sequence of [REGEXP ACTION].  REGEXP is a regular
expression to search for in the output buffer.  ACTION is a function
to be called on each match.  It can access the match data.  Both
REGEXP and ACTION is byte-compiled during macro expansion.  Optionally,
PATTERNS can also be a sequence of pairs (TYPE . RX), where TYPE is
the TYPE parameter of `flymake-make-diagnostic', RX is the regexp in
`rx' notation.

If FILTER-COLOR is non-nil, ANSI color sequences in the output of the
process is filtered out by calling `ansi-color-filter-region'.
This is generally unnecessary because the `:connection-type' argument of
the internal `make-process' call is set to `pipe', so most programs will
not colorize their output.

If DEBUG is non-nil, the process buffer is displayed when the
process terminates.  Otherwise, the buffer is killed silently."
  (let* ((compiled-patterns
          (cl-loop for rule being the elements of patterns
                   collect (flymake-define--compile-rule rule))))
    (byte-compile
     (cl-function
      (lambda (&key report-fn source file cleanup)
        (lambda (p _ev)
          (when (memq (process-status p) '(exit signal))
            (unwind-protect
                (if (eq p (buffer-local-value proc-var source))
                    (with-current-buffer (process-buffer p)
                      (when filter-color
                        (ansi-color-filter-region (point-min) (point-max)))
                      (goto-char (point-min))
                      (let ((diags
                             (cl-loop
                              for (regexp . action) in compiled-patterns
                              do (goto-char (point-min))
                              nconc
                              (cl-loop
                               while (re-search-forward regexp nil t)
                               for diag = (funcall action source file)
                               collect diag))))
                        (if (or diags (zerop (process-exit-status p)))
                            (funcall report-fn diags)
                          (funcall report-fn
                                   :panic :explanation
                                   (buffer-substring
                                    (point-min)
                                    (progn (goto-char (point-min))
                                           (line-end-position)))))))
                  (flymake-log :warning "Cancelling obsolete check %s" p))
              (unless (process-live-p p)
                (when debug (display-buffer (process-buffer p)))
                (unless debug (kill-buffer (process-buffer p)))
                (funcall cleanup))))))))))

;;;; Public macros

;;;###autoload
(cl-defmacro flymake-define
    (name &key documentation command
          (input :stdin) patterns filter-color debug
          no-namespace condition)
  "Define a new flymake diagnostic function.
The diagnostic function is created and bound to symbol NAME both as
function definition and variable value.  The diagnostic function, when
invoked, creates an external process that runs COMMAND and pipes the
outputs to a temporary buffer.  PATTERNS specifies the regexps to search
in the output buffer and actions to be performed on each match.

COMMAND is a sequence of strings, which may optional contain `:input' if
INPUT is `:stdin'.

When INPUT is specified as `:file', the content of the buffer is passed
to the external process via a temporary file.  If the buffer is not
modified, the original file may by used instead of a temporary file.
The input file can be referenced in COMMAND by using `:input'.  When
INPUT is `:stdin' or omitted, the content is passed by standard input.

PATTERNS is a sequence of [REGEXP ACTION].  REGEXP is a regular
expression to search for in the output buffer.  ACTION is a function to
be called on each match.  It can access the match data.  Both REGEXP and
ACTION is byte-compiled during macro expansion.

If FILTER-COLOR is non-nil, ANSI color sequences in the output of the
process is filtered out by calling `ansi-color-filter-region'.  This is
generally unnecessary because the `:connection-type' argument of the
internal `make-process' call is set to `pipe', so most programs will not
colorize their output.

If DEBUG is non-nil, the output buffer is displayed when the
process terminates.  Otherwise, the buffer is killed silently.

If NO-NAMESPACE is non-nil, the \"unshare\" program and mount namespaces
are not used.  This is useful for platforms other than GNU/Linux.  See
the man page `unshare(1)' and the man page `mount_namespaces'.

DOCUMENTATION is the doc-string of the diagnostic function.

If CONDITION is specified, the diagnostic function will report a
`:panic' if the condition is not met.  CONDITION can be either a
function or a form."
  (declare (indent 1))
  (let* ((name-string (symbol-name name))
         (proc-var (intern (concat name-string  "--proc"))))
    `(progn
       (defvar-local ,proc-var nil)
       (defun ,name (report-fn &rest _args)
         ,documentation
         (when (process-live-p ,proc-var)
           (kill-process ,proc-var))
         (if (not ,(cond ((null condition)
                          t)
                         ((memq (car-safe condition) '(function symbol-function lambda))
                          `(funcall ,condition))
                         (t
                          condition)))
             (funcall report-fn
                      :panic :explanation
                      "Condition unmet")

           (let* ((source (current-buffer))
                  (tmpfile (make-temp-file
                            ,name-string nil
                            (ignore-errors
                              (file-name-extension
                               (buffer-file-name) t))))
                  (file (or (buffer-file-name) tmpfile))
                  (cleanup (lambda () (when tmpfile
                                        (delete-file tmpfile)
                                        (setf tmpfile nil))))
                  (cmd ,(cond ((symbolp command)
                               `(append (,command) nil))
                              ((memq (car-safe command) '(function symbol-function lambda))
                               `(append (funcall ,command) nil))
                              (t
                               `(list ,@(append command nil))))))
             (write-region nil nil tmpfile nil 0)

             (when-let* ((cell (memq :input cmd)))
               (setcar cell file))

             (condition-case err
                 (save-restriction
                   (widen)
                   (setq
                    ,proc-var
                    (flymake-define--make-process-in-mntns
                     :mount-points (and file tmpfile (not ,no-namespace)
                                        `((,file ,tmpfile "ro,bind")))
                     :name ,name-string
                     :buffer (generate-new-buffer ,(format " *%s*" name))
                     :command cmd
                     :noquery t
                     :connection-type 'pipe
                     :sentinel
                     (funcall
                      ,(flymake-define--sentinel-factor
                        :filter-color filter-color
                        :debug debug
                        :patterns patterns
                        :proc-var proc-var)
                      :report-fn report-fn
                      :source source
                      ,@(and (eq input :file)
                             '(:file file))
                      :cleanup cleanup)))
                   ,@(if (eq input :stdin)
                         `((process-send-region
                            ,proc-var
                            (point-min) (point-max))
                           (process-send-eof ,proc-var))))
               ((debug t)
                (funcall cleanup)
                (signal (car err) (cdr err)))))))
       (setq-default ,name (symbol-function',name)))))

(provide 'flymake-define)
;;; flymake-define.el ends here
