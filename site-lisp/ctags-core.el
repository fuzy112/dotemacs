;;; ctags-core.el --- ctags support         -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2023, 2024, 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: tools, programming
;; Version: 0.5.5

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; This elisp library provides core facilities for using
;; Universal-Ctags in Emacs.  It includes macros for defining tag
;; structures and functions, helper functions for managing tags files,
;; and commands for creating tags files.  Additionally, it handles
;; parsing output from `readtags` and converting patterns in tags to
;; regex.

;; By default, this package takes advantage of project.el to detect
;; project roots.  If you prefer projectile, add the following code to
;; your configuration:
;;
;;   (defun ctags-projectile-project-function (may-prompt)
;;      (let ((projectile-require-project-root
;;             (and may-prompt \\='prompt)))
;;         (projectile-acquire-root)))
;;
;;   (setq ctags-project-function #\\='ctags-projectile-project-function)
;;

;;; Code:

;;;; Requirements

(eval-when-compile (require 'cl-lib))

;;;; Customization Options

(defgroup ctags nil
  "Ctags support."
  :group 'tools
  :prefix "ctags-")

(defcustom ctags-default-tags-file-name ".tags"
  "Default name to use for tags file."
  :type 'string)

(defcustom ctags-create-tags-default-args
  " -o %TAGSFILE% --languages=all --kinds-all=* --fields=* \
--extras=* --exclude=moc_*.cpp  -R ."
  "Default arguments used to create tags file."
  :type 'string)

(defcustom ctags-universal-ctags-program
  (or (executable-find "ctags-universal")
      (executable-find "ctags"))
  "Path to Universal-Ctags program."
  :type 'file)

(defcustom ctags-readtags-program "readtags"
  "Path to `readtags (1)' program."
  :type 'file)

(defcustom ctags-readtags-always-local 'auto
  "When set to t, always copy remote tags file to the local machine.
When set to nil, run `readtags' on the remote machine."
  :type '(choice (const :tag "Copy tags file to the local machine" t)
                 (const :tag "Run `readtags' on the remote machine" nil)
                 (const :tag "Automatically detected" auto)))

(defcustom ctags-project-function #'ctags--default-project-function
  "Function which return project root directory.
The function takes one boolean argument MAY-PROMPT.  If MAY-PROMPT is non-nil,
the function may ask the user for a project directory."
  :type `(choice (const :tag "Default" ,#'ctags--default-project-function)
                 (function :tag "Custom function")
                 (const :tag "No project integration" nil)))

;;;; Variables

(defvar ctags-history)

;;;; Macros


(defmacro ctags--define-parse-line (name &rest fields)
  "Define parser function.
The parse line function will be NAME concatenated with \"parse-line\"
and only FIELDS will be extracted."
  `(defun ,(intern (format "%s-parse-line" name)) (line)
     "Parse LINE produced by `readtags (1)' into tag properties."
     (if-let* ((tag-end (string-search "\t" line))
               (input-end (string-search "\t" line (+ 1 tag-end))))
         (let (delimiter number number-end pattern pattern-end content)
           (pcase (substring line (+ 1 input-end) (+ 2 input-end))
             ("/" (setq delimiter "/"))
             ("?" (setq delimiter "?"))
             (_   (setq number-end (string-search ";" line input-end)
                        number (substring line (+ 1 input-end) number-end))
                  (if number-end
                      (pcase-exhaustive (substring line (+ 1 number-end) (+ 2 number-end))
                        ("/" (setq delimiter "/"))
                        ("?" (setq delimiter "?"))
                        ("\"" (setq delimiter nil))))))
           (if (null delimiter)
               (setq pattern-end number-end)
             (let ((pattern-begin (if number (+ 2 number-end)
                                    (+ 2 input-end))))
               (if (string-match (concat "[^\\]\\(\\\\\\\\\\)*\\(" delimiter "\\);\"\t") line pattern-begin)
                   (pcase-setq pattern-end (match-beginning 2)
                               `(,pattern . ,content) (ctags--convert-pattern (substring line pattern-begin pattern-end) delimiter))
                 (if (string-match (concat "[^\\]\\(\\\\\\\\\\)*\\(" delimiter "\\)$") line pattern-begin)
                     (pcase-setq pattern-end (match-beginning 2)
                                 `(,pattern . ,content) (ctags--convert-pattern (substring line pattern-begin pattern-end) delimiter))
                   (if (string-prefix-p "!" line)
                       nil
                     (error "Invalid pattern: `%s'" line))))))
           (let ((tag-name (substring-no-properties line 0 tag-end))
                 (input (substring-no-properties line (+ 1 tag-end) input-end))
                 (props (,(intern (format "make-%s" name))))
                 (pos pattern-end))
             (while (string-match "\t\\([[:alnum:]_-]+\\):\\([^\t]*\\)"
                                  line pos)
               (let ((slot-name (intern (match-string-no-properties 1 line)))
                     (slot-value (match-string 2 line)))
                 (when-let* ((pos (seq-position ',fields slot-name)))
                   (setf (aref props (1+ pos)) slot-value)))
               (setq pos (match-end 0)))
             (if-let* ((line-str (,(intern (format "%s-line" name)) props)))
                 (setf (,(intern (format "%s-line" name)) props) (string-to-number line-str))
               (if number
                   (setf (,(intern (format "%s-line" name)) props) (string-to-number number))
                 (setf (,(intern (format "%s-line" name)) props) 1)))
             (setf (,(intern (format "%s-pattern" name)) props) pattern
                   (,(intern (format "%s-content" name)) props) content
                   (,(intern (format "%s-input" name)) props) (expand-file-name input)
                   (,(intern (format "%s-name" name)) props) tag-name)
             props))
       (error "Invalid tag format: %s" line))))


(defconst ctags--increased-gc-cons-threshold (* 64 1024 1024))
(defconst ctags--increased-gc-cons-percentage 0.5)

(defmacro ctags--with-increased-gc (&rest body)
  "Evaluate BODY with increased GC thresholds."
  (declare (indent 0))
  `(let ((gc-cons-threshold ctags--increased-gc-cons-threshold)
         (gc-cons-percentage ctags--increased-gc-cons-percentage))
     ,@body))

(defmacro ctags--with-process (proc-var init-form &rest body)
  "Initialize PROC-VAR with INIT-FORM, then execute BODY."
  (declare (indent 2))
  `(let (,proc-var)
     (unwind-protect
         (progn
           (let ((throw-on-input nil)
                 (inhibit-quit t))
             (setq ,proc-var ,init-form))
           ,@body)
       (let ((throw-on-input nil)
             (inhibit-quit t))
         (when (process-live-p ,proc-var)
           (delete-process ,proc-var))))))

(defmacro ctags--define-readtags (name)
  "Define the function to execute readtags and parse its output.
For the meaning of NAME, see `ctags-define-tag'."
  `(defun ,(intern (format "%s-readtags" name)) (&rest args)
     (let* ((tags-path (ctags-tags-file-path))
            (local-tags-path (if (if (eq ctags-readtags-always-local 'auto)
                                     (setq-local ctags-readtags-always-local
                                                 (if (executable-find "readtags" t)
                                                     nil
                                                   t))
                                   ctags-readtags-always-local)
                                 (or (ctags--file-local-copy-cached tags-path)
                                     tags-path)
                               (file-local-name tags-path)))
            (use-file-handler (not ctags-readtags-always-local))
            (dir (ctags--project-root)))
       (with-current-buffer (get-buffer-create (format " *ctags : %s*" (ctags--project-name dir)))
         (erase-buffer)
         (setq default-directory dir)
         (ctags--with-process proc (make-process
                                    :name "readtags"
                                    :command (append (list ctags-readtags-program
                                                           "-t" local-tags-path)
                                                     args)
                                    :buffer (current-buffer)
                                    :stderr (get-buffer-create " *readtags stderr*")
                                    :sentinel #'ignore
                                    :connection-type 'pipe
                                    :file-handler use-file-handler)
           (while (process-live-p proc)
             (accept-process-output proc 1 nil t))
           (goto-char (process-mark proc))
           (let (tags)
             (dolist (str (string-lines
                           (buffer-substring (point-min)
                                             (line-end-position 0))
                           t))
               (let* ((tag (,(intern (format "%s-parse-line" name)) str))
                      (tag-name (,(intern (format "%s-name" name)) tag)))
                 (cond ((equal tag-name  "!_TAG_PROC_CWD")
                        (setq default-directory
                              (concat (file-remote-p dir)  (,(intern (format "%s-input" name)) tag))))
		       ;; FIXME: handle other pseudo tags
		       ;; _TAG_KIND_DESCRIPTION, etc.
                       ((string-prefix-p "!_" tag-name))
                       (t (push tag tags)))))
             (nreverse tags)))))))


;; Put all above together.
(defmacro ctags-define-tag (name &rest fields)
  "Define a tag structure.

NAME is used to derived names of constructors and accessors.
FIELDS are the fields that the tag may store.

The following functions will be defined:

NAME-make: the constructor of the tag, which accept no arguments.

NAME-FIELD: accessor for field named FIELD.  It takes one argument,
the tag.  It is a generalized variable.

NAME-parse-line: function used internally to parse outputs of
`readtags'.

NAME-readtags: read tags using `readtags'.  It takes any number of
command line arguments and passes them to `readtags'.

See also man page `readtags(1)'."
  (declare (indent 1))
  (macroexp-progn `( (cl-defstruct ,name
                       ,@fields)
                     (ctags--define-parse-line ,name ,@fields)
                     (ctags--define-readtags ,name))))

;;;; Helper functions

(defvar ctags--tags-file-path nil)

(defun ctags--contains-tags-file-p (dir)
  "Return t if a tags exists in DIR."
  (cl-dolist (name '(".tags" "tags") nil)
    (let ((path (expand-file-name name dir)))
      (and (file-exists-p path)
           (setq-local ctags--tags-file-path path)
           (cl-return t)))))

;;;###autoload
(defun ctags-tags-file-path ()
  "Return the absolute path of the tags file or nil."
  (cond ((local-variable-p 'ctags--tags-file-path)
         ctags--tags-file-path)
        (t
         (if (locate-dominating-file default-directory #'ctags--contains-tags-file-p)
	     ctags--tags-file-path
           (setq-local ctags--tags-file-path nil)))))


(defun ctags--convert-pattern (pattern delimiter)
  "Convert PATTERN in tags to a pair of regexp and content summary.
Occurrences of DELIMITER in PATTERN will be quoted."
  (let ((begin "")
        (end "")
        (content pattern))
    (when (string-prefix-p "^" content)
      (setq begin "^"
            content (substring content 1)))
    (when (string-match "\\(\\`\\|[^\\]\\(\\\\\\\\\\)*\\)\\$\\'" content)
      (setq end "$"
            content (substring content 0 -1)))
    (setq content (string-replace "\\\\" "\\" content))
    (setq content (replace-regexp-in-string "\\\\\\$\\'" "$" content))
    (setq content (string-replace (concat "\\" delimiter) delimiter content))
    (cons (concat begin (regexp-quote content) end)
          content)))

(byte-compile 'ctags--convert-pattern)


(defvar ctags--cached-file-local-copies (make-hash-table :test 'equal)
  "Cache info for file local copies.
The keys are remote file names.
The values are paths to the local cache files.")

;;;###autoload
(defun ctags--file-local-copy-cached (file)
  "Like `file-local-copy', but use cached result if FILE is not changed."
  (if (not (file-remote-p file))
      nil
    (let* ((attrs (file-attributes file))
           (modtime (file-attribute-modification-time attrs))
           (size (file-attribute-size attrs)))
      (if-let* ((cache-file (gethash file ctags--cached-file-local-copies))
                (cache-file-attrs (file-attributes cache-file))
                (cache-file-modtime (file-attribute-modification-time cache-file-attrs))
                (cache-file-size (file-attribute-size cache-file-attrs)))
          (progn
            (when (or (/= cache-file-size size)
                      (time-less-p cache-file-modtime modtime))
              (copy-file file cache-file 'ok-if-already-exists 'keep-time
                         'preserve-uid-gid))
            cache-file)
        (setq cache-file (file-local-copy file))
        (puthash file cache-file ctags--cached-file-local-copies)
        cache-file))))


(declare-function project-root "project")
(declare-function project-current "project")

(defun ctags--default-project-function (may-prompt)
  (and-let* ((p (project-current may-prompt)))
    (project-root p)))

(defun ctags--project-root (&optional may-prompt)
  (and-let* ((root (and (functionp ctags-project-function)
                        (funcall ctags-project-function may-prompt))))
    (expand-file-name root)))

(defun ctags--project-name (dir)
  (if (string-match "/\\([^/]+\\)/\\'" dir)
      (propertize (match-string 1 dir) 'help-echo (abbreviate-file-name dir))
    dir))

(defun ctags--create-tags-default-command ()
  "Return the default command used to create the tags table."
  (let* ((dir (ctags--project-root (ctags-tags-file-path)))
         (file-name
          (if (ctags-tags-file-path)
              (file-relative-name
               (file-local-name (ctags-tags-file-path))
               dir)
            ctags-default-tags-file-name)))
    (concat ctags-universal-ctags-program
            " "
            (string-replace "%TAGSFILE%"
                            file-name
                            ctags-create-tags-default-args))))

;;;; Commands

;;;###autoload
(defun ctags-create-tags (dir command-args)
  "Create a tags file in DIR with COMMAND-ARGS.

When called interactively, DIR is determined by `ctags--project-root'
and COMMAND-ARGS is is read from minibuffer.

DIR can also be read from minibuffer if the command is called with a
prefix argument.

If DIR is a remote directory, the `ctags' command will be executed on
the remote machine.

See also man page `ctags(1)'."
  (interactive
   (let* ((dir (ctags--project-root))
          (dir2
           (if current-prefix-arg
               (read-directory-name
                "Run ctags in this directory: "
                dir)
             dir))
          (default (ctags--create-tags-default-command)))
     (list dir2
           (read-shell-command
            (format "Run ctags (in %s): " (ctags--project-name dir2))
            default
            'ctags-history
            default))))
  (let ((default-directory dir))
    (compilation-start command-args)))

(provide 'ctags-core)
;;; ctags-core.el ends here
