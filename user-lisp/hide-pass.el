;;; hide-pass.el --- Hide passwords in buffer        -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024, 2025  Zhengyi Fu <i@fuzy.me>

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: convenience
;; Version: 0.4.6

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

;; This package provides a minor mode to hide plain text passwords in
;; a buffer.

;;; Code:

(require 'compat nil t)
(require 'reveal)

(defgroup hide-pass nil
  "Hide passwords in buffer with overlays."
  :group 'files)

(defface hide-pass
  '((t :inherit font-lock-doc-face))
  "Face used to hide passwords.")

(defcustom hide-pass-regexps
  '("\\_<\\(?:hide-pass\\|[Pp]assword:?\\)[[:space:]]*\"\\(.+\\)\""
    "JENKINS_PASS=\\([a-zA-Z0-9]+\\)"
    "\\_<\\[Pp]assword:[[:space:]]+\\([^[:space:]]+\\)")
  "Regexp to match a password."
  :type '(repeat string)
  :safe #'list-of-strings-p)

(defconst hide-pass-mask (propertize "****" 'face 'font-lock-doc-face)
  "The string to display on a overlay to hide secrets.")

(defvar-local hide-pass--point nil
  "The last point position after a command is execute.")

(defun hide-pass--toggle-display (overlay hide)
  "Temporarily toggle the OVERLAY.
If HIDE is non-nil, hide the region using the overlay.
Otherwise, the region is displayed."
  (overlay-put overlay
               'display (if hide
                            hide-pass-mask
                          nil)))

;; Symbol `hide-pass' is used as the value of `category' properties of
;; the overlays.  Properties of `hide-pass' will be inherited by those
;; overlays.
(put 'hide-pass 'display hide-pass-mask)
(put 'hide-pass 'face 'hide-pass)
(put 'hide-pass 'evaporate t)
(put 'hide-pass 'reveal-toggle-invisible #'hide-pass--toggle-display)

(defun hide-pass--post-command ()
  "Update `hide-pass--point'."
  (setq hide-pass--point (point)))

(defun hide-pass--delete-if (pred list)
  "Remove all items satifying PRED from LIST.

This is a destructive function; it reuses the storage of LIST
whenever possible."
  (let (prev (tail list))
    (while tail
      (if (funcall pred (car tail))
          (if prev
              (setcdr prev (cdr tail))
            (setq list (cdr tail)))
        (setq prev tail))
      (setq tail (cdr tail))))
  list)

(defun hide-pass--overlays-in (beg end)
  "Return all `hide-pass' overlays in region between BEG and END."
  (hide-pass--delete-if
   (lambda (ol)
     (not (eq (overlay-get ol 'category) 'hide-pass)))
   (overlays-in beg end)))

(defun hide-pass--hide-region (beg end)
  "Make an overlay at region (BEG . END) to hide the secrets.
If there are already overlays with `hide-pass' category in this
region, they are moved or deleted."
  (pcase (hide-pass--overlays-in beg end)
    ('nil
     (let ((ol (make-overlay beg end nil nil t) ))
       (overlay-put ol 'category 'hide-pass)
       (if (and (bound-and-true-p reveal-auto-hide)
                (<= hide-pass--point (+ 1 end))
                (>= hide-pass--point (- beg 1)))
           (push `((selected-window) . ,ol) reveal-open-spots)
         (hide-pass--toggle-display ol t))))
    (`(,ol . ,other-ols)
     (move-overlay ol beg end)
     (dolist (ol other-ols)
       (delete-overlay ol)))))

(defun hide-pass--fontify (beg end)
  "Make overlays for region (BEG . END)."
  (save-excursion
    (save-match-data
      (dolist (re hide-pass-regexps)
        (goto-char beg)
        (while (re-search-forward re end 'noerror)
          (hide-pass--hide-region (match-beginning 1)
                                  (match-end 1)))))
    `(jit-lock-bounds ,beg . ,end)))

;;;###autoload
(define-minor-mode hide-pass-mode
  "Hide passwords in buffer with ****."
  :lighter " ***"
  :after-hook (font-lock-flush)
  (cond (hide-pass-mode
         (add-hook 'post-command-hook #'hide-pass--post-command nil t)
         (jit-lock-register #'hide-pass--fontify))
        (t
         (remove-hook 'post-command-hook #'hide-pass--post-command t)
         (jit-lock-unregister #'hide-pass--fontify)
         (without-restriction
           (remove-overlays (point-min) (point-max)
                            'category 'hide-pass)))))

(defun hide-pass-mode-turn-on ()
  "Turn on `hide-pass-mode'."
  (when (derived-mode-p 'prog-mode 'text-mode)
    (hide-pass-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-hide-pass-mode hide-pass-mode
  hide-pass-mode-turn-on)

;;;###autoload
(progn
  (defun hide-pass (password)
    "A dummy function used to hide PASSWORD.
This function returns PASSWORD directly."
    password))


;;;; Test examples
;; ("--password" "abcdefABC1*")
;; ("--password" "test123\"T")
;; (hide-pass "my-password")

(provide 'hide-pass)
;;; hide-pass.el ends here
