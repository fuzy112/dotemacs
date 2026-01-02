;;; fuzzy-finder.el --- Fuzzy-Finder command line builder            -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: tools

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

;; This package provide a unified interface for building commands for `fzf' and `skim'.

;;; Code:

(require 'eieio)

(defclass fuzzy-finder
  ()
  ((ansi		:initform  t					:initarg :ansi)
   (bind		:initform  nil					:initarg :bind)
   (cmd			:initform  "fd . || fd-find . || find ."	:initarg :cmd)
   (case		:initform  'smart				:initarg :case)
   (cmd-prompt		:initform  "c> "				:initarg :cmd-prompt)
   (cmd-query		:initform  ""					:initarg :cmd-query)
   (delimiter		:initform  "[[:space:]]+"			:initarg :delimiter)
   (header		:initform  ""					:initarg :header)
   (header-lines	:initform 0					:initarg :header-lines)
   (height		:initform  "100%"				:initarg :height)
   (layout		:initform  "default"				:initarg :layout)
   (interactive		:initform  nil					:initarg :interactive)
   (prompt		:initform  "> "					:initarg :prompt)
   (preview		:initform  "true"				:initarg :preview)
   (preview-window	:initform "right:50%"				:initarg :preview-window))
  :abstract t
  :documentation "Bass class of all fuzzy-finders.")

(defclass fuzzy-finder-skim (fuzzy-finder) ()
  :documentation "Command builder for `skim'.")

(defclass fuzzy-finder-fzf (fuzzy-finder) ()
  :documentation "Command builder for `fzf'.")

(cl-defmethod fuzzy-finder-build ((obj fuzzy-finder-skim))
  (let ((cmd (replace-regexp-in-string "{}" "'{}'" (oref obj cmd) t t)))
    (mapconcat
     #'shell-quote-argument
     (flatten-tree
      (list (executable-find "sk")
	    (and (oref obj ansi) "--ansi")
	    (mapcar (lambda (bind) (list "--bind" bind))
		    (oref obj bind))
	    "--cmd" cmd
	    "--case" (symbol-name (oref obj case))
	    "--cmd-prompt" (oref obj cmd-prompt)
	    "--delimiter" (oref obj delimiter)
	    "--header" (oref obj header)
	    "--header-lines" (number-to-string (oref obj header-lines))
	    "--height" (oref obj height)
	    "--layout" (oref obj layout)
	    (and (oref obj interactive) "-i")
	    "--prompt" (oref obj prompt)
	    "--preview" (oref obj preview)
	    "--preview-window" (oref obj preview-window)))
     " ")))

(cl-defmethod fuzzy-finder-build ((obj fuzzy-finder-fzf))
  (let ((cmd (and (oref obj cmd)
		  (concat (replace-regexp-in-string "{}" "{q}" (oref obj cmd) t t)
			  " || :"))))
    (mapconcat
     #'shell-quote-argument
     (flatten-tree
      (list (executable-find "fzf")
	    (and (oref obj ansi) "--ansi")
	    (mapcar (lambda (bind) (list "--bind" bind))
		    (oref obj bind))
	    "--delimiter" (oref obj delimiter)
	    "--header" (oref obj header)
	    "--header-lines" (number-to-string (oref obj header-lines))
	    "--layout" (oref obj layout)
	    "--preview" (oref obj preview)
	    "--preview-window" (oref obj preview-window)
	    (if (oref obj interactive)
		(list "--prompt" (oref obj cmd-prompt)
		      "--disabled"
		      "--bind" (format "change:reload(sleep 0.4; %s)" cmd))
	      (list "--prompt" (oref obj prompt)))
	    (and cmd (list "--bind" (format "start:reload(%s)" cmd)))
	    "--bind" (format "ctrl-q:unbind(change)+clear-query+change-prompt(%s)+enable-search"
			     (oref obj prompt))
	    "--bind" (format "ctrl-j:clear-query+rebind(change)+change-prompt(%s)+disable-search"
			     (oref obj cmd-prompt))))
     " ")))

(defvar fuzzy-finder-default-class
  (cond ((executable-find "sk") 'fuzzy-finder-skim)
	((executable-find "fzf") 'fuzzy-finder-fzf)
	(t (display-warning 'fuzzy-finder "Please install `fzf' or `skim'")))
  "Default fuzzy-finder command builder to use when not specified.")

;;;###autoload
(cl-defun fuzzy-finder-command (&rest args &key class &allow-other-keys)
  (unless class
    (setq class fuzzy-finder-default-class))
  (concat
   (fuzzy-finder-build
    (apply #'make-instance class args))
   " || :"))

(provide 'fuzzy-finder)
;;; fuzzy-finder.el ends here
