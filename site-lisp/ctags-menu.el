;;; ctags-menu.el --- Transient menu for Ctags -*- lexical-binding: t -*-
;; Copyright © 2024  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.3
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

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
;;; Code:

;;;; Requirements

(require 'ctags-core)
(require 'transient)

;;;;

(defvar ctags-menu--directory)

;;;###autoload(autoload 'ctags-menu "ctags-menu" nil t)
(transient-define-prefix ctags-menu ()
  [4 "Input/Output Options"
     (ctags-menu:--)
     ("-x" "Exclude files and directories matching <pattern>" "--exclude="
      :multi-value t)
     ("-X" "Don't exclude files and directories matching <pattern>" "--exclude-exception=")
     ("-l" "Symbolic links should be followed" "--links=" :choices ("yes" "no") :level 3)
     ("-m" "Specify maxium recursion depth" "--maxdepth=" :level 5)
     ("-R" "Recurse into directories supplied on command line" ("-R" "--recurse") :level 2)
     ("-L" "A list of input file name is read from the specified file" "-L"
      :class transient-option :level 4
      :unsavable t)
     ("-a" "Append the tags to an existing tag file" ("-a" "--append") :level 5)
     (ctags-menu:-o)]
  [5 "Output Format Options"
     ("-f" "Force output of specified tag file format" "--format=" :choices ("1" "2"))
     ("-F" "Specify the output format" "--output-format=" :choices ("u-ctags" "e-ctags" "etags" "xref" "json"))
     ("-b" "Print a tabular cross reference file" "-x")
     ("-s" "Should tags be sorted?" "--sort=" :choices ("yes" "no" "foldcase"))
     ("-e" "Include reference to file in Emacs-style tag file" "--etags-include=")
     ("-i" "Specify encoding for of all input files" "--input-encoding=")
     (ctags-menu:-I)
     ("-O" "The enocding to write the tag file in." "--output-encoding=")]
  [4 "Language Selection and Mapping Options"
     (ctags-menu:languege-force)
     (ctags-menu:languages)
     (ctags-menu:-A)
     ("-g" "Guess languages eagerly" ("-G" "--guess-languages-eagerly"))
     (ctags-menu:langmap)
     (ctags-menu:map)]
  [4 "Tags File Contents Options"
     ("-c" "Use the specified type of EX command to locate tags" "--excmd="
      :choices ("number" "pattern" "mix" "combine") :level 6)
     ("-t" "Include extra tag entries for selected information" "--extras=")
     ("-T" "Include language own extra tag entries for selected information" "--extras-"
      :class transient-option :reader ctags-menu--read-languages-and-value)
     ("-d" "Include selected extension fields" "--fields=")
     ("-D" "Include selected language own extension fields" "--fields-"
      :class transient-option :reader ctags-menu--read-languages-and-value)
     ("-k" "Enable tag kinds for language" "--kinds-"
      :class transient-option
      :reader ctags-menu--read-languages-and-value)
     ("-p" "Pattern length limimt" "--pattern-length-limit=" :level 5)
     ("-P" "Emit pseudo tags" "--pseudo-tags=" :multi-value t)
     ("-r" "Enable tag roles for kindcs of language" "--roles-"
      :class transient-option
      :reader ctags-menu--read-languages-and-value)
     ("-v" "Use relative path" "--tag-relative=" :choices ("yes" "no" "always" "never"))
     ("-B" "Use backword seaching patterns" "-B" :level 5)
     ("-F" "Use forward searching patterns" "-F" :level 5)]
  [5 "Option File Options"]
  [1 "Commmand"
     (ctags-menu:RET)
     (ctags-menu:edit-and-run)]
  (interactive)
  (let ((dir (ctags--project-root t)))
    (with-temp-buffer
      (setq default-directory dir)
      (setq ctags-menu--directory default-directory)
      (transient-setup 'ctags-menu nil nil))))



(defun ctags-menu--args ()
  (let* ((default-directory ctags-menu--directory)
         (args (transient-get-value)))
    (append (seq-filter #'atom args)
            (and-let* ((pseudo-tags (assoc "--pseudo-tags=" args)))
              (list (concat "--pseudo-tags=" (string-join (cdr pseudo-tags) ","))))
            (and-let* ((languages (assoc "--languages=" args)))
              (list (concat "--languages=" (string-join (cdr languages) ","))))
            (and-let* ((langmap (assoc "--langmap=" args)))
              (list (concat "--langmap=" (string-join (cdr langmap) ","))))
            (and-let* ((excludes (assoc "--exclude=" args)))
              (mapcar (lambda (arg) (concat "--exclude=" arg)) (cdr args)))
            (or (alist-get "--" args nil nil 'equal)
                (list ".")))))

(defun ctags-menu--languages ()
  (process-lines "ctags" "--list-languages"))

(defun ctags-menu--format-prompt-for-language (prompt language)
  (replace-regexp-in-string
   "-:" (concat "-" language ":") prompt t t))

(defun ctags-menu--read-languages-and-value (prompt initial-input history)
  (ignore prompt initial-input history)
  (let* ((languages (cons "all" (ctags-menu--languages)))
         (lang (completing-read prompt languages nil t))
         (value (read-string (ctags-menu--format-prompt-for-language prompt lang))))
    (concat lang "=" value)))


(transient-define-infix ctags-menu:-- ()
  :level 2
  :description "Input files"
  :argument "--"
  :key "--"
  :class 'transient-files
  :reader #'ctags-menu--read-files
  :init-value #'ctags-menu:---init-value
  :unsavable t
  :multi-value t)

(defun ctags-menu:---init-value (option)
  (when-let* ((default-directory ctags-menu--directory)
              (project (project-current)))
    (setf (oref option value) (cons (file-relative-name (project-root project))
                                    (project-external-roots project)))))

(defun ctags-menu--read-files (prompt initial-input hist)
  (let ((default-directory ctags-menu--directory))
    (completing-read-multiple prompt (directory-files-recursively "." "." t)
                              #'file-exists-p t initial-input hist)))

(transient-define-infix ctags-menu:-o ()
  :key "-o"
  :description "Write tags to specified file."
  :argument "-o"
  :shortarg "-f"
  :class 'transient-option
  :unsavable t
  :init-value #'ctags-menu:-o-init-value)

(defun ctags-menu:-o-init-value (obj)
  (when-let ((file (ctags-tags-file-path)))
    (setf (oref obj value) (file-relative-name file))))

(transient-define-infix ctags-menu:-I ()
  :description "Specify encoding for files of specified language"
  :key "-I"
  :class 'transient-option
  :argument "--input-encoding-"
  :reader #'ctags-menu--read-languages-and-value)

(transient-define-infix ctags-menu:languege-force ()
  :description "Force all files to be interpreted as specified language"
  :key "-n"
  :class 'transient-option
  :argument "--language-force="
  :choices (ctags-menu--languages))

(transient-define-infix ctags-menu:languages ()
  :level 4
  :description "Restrict files scanned for tags to specified languanes"
  :key "-l"
  :class 'transient-option
  :argument "--languages="
  :multi-value t
  :choices (cons "all" (ctags-menu--languages)))

(transient-define-infix ctags-menu:-A ()
  :description "Add a pattern for language"
  :key "-A"
  :argument "--alias-"
  :level 5
  :class 'transient-option
  :reader #'ctags-menu--read-languages-and-value)

(transient-define-infix ctags-menu:langmap ()
  :description "Override default mapping of language to input file extensions"
  :key "-m"
  :argument "--langmap="
  :class 'transient-option
  :multi-value t
  :level 5
  :choices (ctags-menu--languages))

(transient-define-infix ctags-menu:map ()
  :description "Set, add(+) or remove(-) the map for language"
  :key "-M"
  :argument "--map-"
  :class 'transient-option
  :level 6
  :reader #'ctags-menu--read-languages-and-value)

(defun ctags-menu--buffer-native (_mode)
  (format "*ctags : %s*" (ctags--project-name default-directory)))

(transient-define-suffix ctags-menu:RET (args)
  :description "Run the command"
  :key "RET"
  (interactive (list (ctags-menu--args)))
  (let ((default-directory ctags-menu--directory))
    (compilation-start (concat "ctags " (mapconcat #'shell-quote-argument args " "))
                       nil #'ctags-menu--buffer-native)))

(transient-define-suffix ctags-menu:edit-and-run (args)
  :description "Edit and run the command"
  :key "!"
  (interactive (list (ctags-menu--args)))
  (let* ((default-directory ctags-menu--directory)
         (command (read-shell-command "Run ctags: " (concat "ctags " (combine-and-quote-strings args " ")))))
    (compilation-start command nil #'ctags-menu--buffer-native)))


(provide 'ctags-menu)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (outline-minor-mode)
;; End:

;;; ctags-menu.el ends here
