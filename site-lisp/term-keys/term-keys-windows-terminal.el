;;; term-keys-windows-terminal.el --- term-keys support for Windows Terminal -*- lexical-binding: t; -*-

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

;; This file contains supplementary code for aiding in the
;; configuration of Windows Terminal terminal emulator to interoperate
;; with the term-keys package.

;; For more information, please see the accompanying README.me file.

;;; Code:

(require 'term-keys)
(require 'json)

(defgroup term-keys/windows-terminal nil
  "`term-keys' options for Windows Terminal."
  :group 'term-keys)

(define-widget 'term-keys/windows-terminal-modifier 'lazy
  "Choice for Windows Terminal key binding modifiers and state flags."
  :type '(choice (const :tag "Shift" "shift+")
		 (const :tag "Ctrl" "ctrl+")
		 (const :tag "Alt" "alt+")
		 (const :tag "Win" "win+")
		 (const :tag "(none)" nil)))

(defcustom term-keys/windows-terminal-modifier-map ["shift+" "ctrl+" "alt+" "win+" nil nil]
  "Modifier keys for Windows Terminal key bindings.

This should be a vector of 6 elements, with each element being a
string indicating the name of the Windows Terminal modifier
corresponding to the Emacs modifiers Shift, Control, Meta, Super,
Hyper and Alt respectively, as they should appear in generated
Windows Terminal settings JSON files.  nil indicates that there
is no mapping for this modifier."
  :type '(vector
	  (term-keys/windows-terminal-modifier :tag "Shift")
	  (term-keys/windows-terminal-modifier :tag "Control")
	  (term-keys/windows-terminal-modifier :tag "Meta")
	  (term-keys/windows-terminal-modifier :tag "Super")
	  (term-keys/windows-terminal-modifier :tag "Hyper")
	  (term-keys/windows-terminal-modifier :tag "Alt")))

(defun term-keys/windows-terminal-json ()
  "Construct Windows Terminal settings.

This function returns, as a string, the Windows Terminal settings
in JSON format used to configure Windows Terminal to encode
term-keys key sequences, according to the term-keys
configuration."
  (let ((actions nil))
    (term-keys/iterate-keys
     (lambda (index keymap mods)
       (unless (cl-reduce (lambda (x y) (or x y))
			  (mapcar (lambda (n)
				    (and (elt mods n)
					 (not (elt term-keys/windows-terminal-modifier-map n))))
				  (number-sequence 0 (1- (length mods)))))
	 (let ((input (concat term-keys/prefix
			      (term-keys/encode-key index mods)
			      term-keys/suffix))
	       (keys (concat (mapconcat
			      (lambda (n)
				(if (elt mods n)
				    (elt term-keys/windows-terminal-modifier-map n)))
			      (number-sequence 0 (1- (length mods))))
			     (elt keymap 14))))
	   (push `((:command . ((:action . "sendInput")
				(:input . ,input)))
		   (:keys . ,keys))
		 actions)))))
    (let ((json-encoding-pretty-print t))
      (json-encode `((:actions . ,(nreverse actions)))))))

(provide 'term-keys-windows-terminal)
;;; term-keys-windows-terminal.el ends here
