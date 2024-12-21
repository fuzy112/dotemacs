;; -*- coding: utf-8-unix; -*-
prog-mode

(copyright (if (derived-mode-p 'lisp-data-mode)
	       ";; "
	     (if (string-suffix-p " " comment-start)
		 comment-start
	       (concat comment-start " ")))
	   "Copyright © "
	   (p (format-time-string "%Y"))
	   "  " (p user-full-name holder)
	   " <" (p user-mail-address mail) ">"
	   comment-end)

(gpl (let ((text "This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
")
	   (mode major-mode))
       (with-temp-buffer
	 (funcall mode)
	 (insert text)
	 (let ((comment-style 'extra-line))
	   (comment-region (point-min) (point-max)))
	 (buffer-string))))

