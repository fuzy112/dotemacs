;;;; nix-elpa-diff.el --- Diff Emacs packages between NixOS generations -*- lexical-binding: t; -*-

;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

;; Author: Zhengyi Fu

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

;;;;

(require 'cl-lib)

(defgroup nix-elpa-diff nil
  "Diff the Emacs package set between Nix system generations."
  :group 'tools
  :prefix "nix-elpa-diff-")

(defcustom nix-elpa-diff-program
  (locate-file "nix-elpa-diff"
	       (cons (or (and load-file-name
			      (file-name-directory load-file-name))
			 default-directory)
		     exec-path)
	       '(".sh"))
  "Path to the nix-elpa-diff script."
  :type 'file
  :group 'nix-elpa-diff)

(defcustom nix-elpa-diff-profiles-directory "/nix/var/nix/profiles"
  "Directory containing the Nix system profile generations."
  :type 'directory
  :group 'nix-elpa-diff)

;; ---------------------------------------------------------------------------
;; Generation helpers
;; ---------------------------------------------------------------------------

(defun nix-elpa-diff--generations ()
  "Return available system generations, youngest first."
  (sort (directory-files nix-elpa-diff-profiles-directory
			 nil "\\`system-[0-9]+-link\\'" t)
	(lambda (a b) (string-version-lessp b a))))

(defun nix-elpa-diff--current-generation ()
  "Return the link name of the current generation, or nil."
  (let ((target (file-symlink-p
		 (expand-file-name "system" nix-elpa-diff-profiles-directory))))
    (and target (file-name-nondirectory target))))

(defun nix-elpa-diff--read-generation (prompt &optional default)
  (completing-read prompt (nix-elpa-diff--generations)
		   nil nil nil nil default))

;; ---------------------------------------------------------------------------
;; Faces (font-lock) — path shortening is done by overlays, see below
;; ---------------------------------------------------------------------------

(defvar nix-elpa-diff-font-lock-keywords
  `(;; Added/removed summary (stderr, merged into the buffer).
    ("^\\(removed\\|added\\) packages ([0-9]+):$"
     (0 'diff-file-header t))
    ;; Our ===== pkg ===== section headers.
    ("^===== \\([^ \n]+\\) =====$"
     (0 'diff-hunk-header t))
    ;; The "diff ..." invocation lines and the --- / +++ file labels.
    ("^diff .*$" (0 'diff-file-header t))
    ("^\\(---\\|\\+\\+\\+\\) .*$" (0 'diff-file-header t)))
  "Font-lock rules for the *diff-pkgs* buffer.  Path shortening is NOT
done here: it needs per-match dynamic replacements, which font-lock
keywords cannot express; see `nix-elpa-diff--prettify'.")

;; ---------------------------------------------------------------------------
;; Path shortening (overlays)
;; ---------------------------------------------------------------------------

(defconst nix-elpa-diff--site-lisp-re
  "\\(/nix/store/[a-z0-9]\\{32\\}-[^/\n]+/share/emacs/site-lisp/elpa/\\)"
  "Regexp matching the store prefix of every path the script emits.")

(defun nix-elpa-diff--shorten (beg end display &optional face)
  "Display text between BEG and END as DISPLAY, via an evaporating overlay."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'display display)
    (when face (overlay-put ov 'face face))
    (overlay-put ov 'evaporate t)))

(defun nix-elpa-diff--side-prefix (side)
  "Return the store prefix used on SIDE (`old' or `new'), or nil.
Skips ---/+++ lines whose path is /dev/null (added/removed files)."
  (save-excursion
    (goto-char (point-min))
    (let ((re (pcase side
		('old "^--- \\(.+?\\)\t")
		('new "^\\+\\+\\+ \\(.+?\\)\t"))))
      (cl-block found
	(while (re-search-forward re nil t)
	  (let ((path (match-string 1)))
	    (when (string-match (concat "\\`" nix-elpa-diff--site-lisp-re) path)
	      (cl-return-from found (match-string 1 path)))))))))

(defun nix-elpa-diff--prettify ()
  "Shorten paths and drop flag/timestamp noise, using display overlays.
The underlying text is untouched; only the rendering changes."
  (save-excursion
    ;; 1. Learn the two site-lisp prefixes from the --- / +++ lines,
    ;;    then replace each occurrence buffer-wide with old/ or new/.
    (let ((old-prefix (nix-elpa-diff--side-prefix 'old))
	  (new-prefix (nix-elpa-diff--side-prefix 'new)))
      (when (and old-prefix new-prefix (not (string= old-prefix new-prefix)))
	(dolist (spec (list (list old-prefix "old" 'diff-removed)
			    (list new-prefix "new" 'diff-added)))
	  (pcase-let ((`(,prefix ,label ,face) spec))
	    (goto-char (point-min))
	    (let ((re (regexp-quote prefix)))
	      (while (re-search-forward re nil t)
		(nix-elpa-diff--shorten (match-beginning 0) (match-end 0)
					(concat label "/") face)))))))
    ;; 2. "diff" lines: drop the flags between "diff" and the first path.
    (goto-char (point-min))
    (while (re-search-forward "^diff\\( .*?\\)/nix/store/" nil t)
      (nix-elpa-diff--shorten (match-beginning 1) (match-end 1) " "))
    ;; 3. --- / +++ lines: drop the tab and timestamp at end of line.
    (goto-char (point-min))
    (while (re-search-forward "^\\(?:---\\|\\+\\+\\+\\) [^\t\n]*\\(\t[^\n]*\\)$"
			      nil t)
      (nix-elpa-diff--shorten (match-beginning 1) (match-end 1) ""))))

;; ---------------------------------------------------------------------------
;; Command
;; ---------------------------------------------------------------------------

;;;###autoload
(defun nix-elpa-diff (old new)
  "Diff Emacs Lisp packages between Nix system generations OLD and NEW.
Interactively, diff the previous generation against the current one.
With a prefix argument, prompt for both generations, with completion
over the available generations; you can also type a bare generation id
like \"41\" or an absolute path."
  (interactive
   (if current-prefix-arg
       (let* ((gens (nix-elpa-diff--generations))
	      (cur (nix-elpa-diff--current-generation))
	      (prev (cadr (member cur gens))))
	 (list (nix-elpa-diff--read-generation "Old generation: " prev)
	       (nix-elpa-diff--read-generation "New generation: " cur)))
     (list nil nil)))
  (let ((buf (get-buffer-create "*diff-pkgs*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
	    status)
	(erase-buffer)
	(diff-mode)
	;; diff-mode sets outline-regexp itself; extend it with our
	;; section headers.  Re-running diff-mode resets all buffer-local
	;; variables, so this cannot accumulate across runs.
	(setq-local outline-regexp
		    (concat outline-regexp "\\|^===== [^ ]+ =====$"))
	(outline-minor-mode 1)
	(font-lock-add-keywords nil nix-elpa-diff-font-lock-keywords)
	;; DESTINATION t: stdout and stderr both land in this buffer,
	;; interleaved in the order the script wrote them.
	(setq status
	      (apply #'call-process nix-elpa-diff-program nil t t
		     (and old new (list old new))))
	(read-only-mode 1)
	(nix-elpa-diff--prettify)
	(goto-char (point-min))
	(when (> status 1)
	  (message "nix-elpa-diff exited with status %d" status))))
    (pop-to-buffer buf)))

(provide 'nix-elpa-diff)
;;; nix-elpa-diff.el ends here
