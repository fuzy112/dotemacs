;; -*- mode: lisp-data; coding: utf-8; -*-

emacs-lisp-mode

(declare-function "(declare-function "
		  (p "func" function)
		  " \""
		  (when-let* ((f (intern function))
			      ((functionp f))
			      (lib (progn (autoload-do-load f)
					  (find-function-library f)))
			      (full-file (cdr-safe lib))
			      (file (file-name-nondirectory full-file)))
		    (if (string-suffix-p ".elc" file)
			(substring file 0 -1)
		      file))
		  "\""
		  (when-let* ((f (intern function))
			      ((functionp f))
			      (args (help-function-arglist f))
			      ((consp args)))
		    (format " %s" (prin1-to-string args)))
		  ")"
		  q)

(autoloadf "(autoload '" (p "function" func) " \""
	   (when-let* ((f (intern func))
		       ((functionp f))
		       (lib (find-function-library f))
		       (full-file (cdr-safe lib))
		       (file (file-name-nondirectory full-file)))
	     (if (string-suffix-p ".elc" file)
		 (substring file 0 -1)
	       file))
	   "\"" n
	   "\""
	   (if (functionp (intern func))
	       (documentation (intern func)))
	   "\" "
	   (if (commandp (intern func))
	       "t")
	   ")"
	   )

(autoload ";;;###autoload")

(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " --- " p " -*- lexical-binding: t -*-" n
	(i copyright) n n
	";; Author: "  "  " user-full-name " <" user-mail-address ">" n
	";; Package-Requires: ((emacs \"" emacs-version "\"))" n
	";; Version: 0.1.0" n
	";; Keywords:" n n
        ";;; Commentary:" n ";;; Code:" n n)

(package (i header) r n n (i provide))

(provide "(provide '" (file-name-base (buffer-file-name)) ")" n
	 ";;; " (file-name-nondirectory (buffer-file-name)) " ends here" n)

(transient-define-prefix "(transient-define-prefix " p " (" p ")" n
  "\"" p "\"" n
  "[" p "])")

(transient-define-infix "(transient-define-infix " p " ()" n
  "\"" p "\"" n
  ":class" p n
  p ")")

(transient-define-suffix "(transient-define-suffix " p " (" p ")" n
  "\"" p "\"" n
  "(interactive" p ")" n
  p ")")
(defgroup "(defgroup " (p (file-name-base (buffer-file-name))) " ()" n>
  "\"" p "\"" n>
  ":group '" (p "") n>
  ":prefix \""  (p (file-name-base (buffer-file-name))) "-\"" ")")


(defcustom "(defcustom " p " " (p "nil") n>
  "\"" p "\"" n>
  ":type '" p ")")

(defvar "(defvar " p " " (p "nil") n>
  "\"" p "\"" ")")

(slot "(" (p "" name) "\t" ":initform " (p "nil") "\t" ":initarg " (concat ":" name) ")")
