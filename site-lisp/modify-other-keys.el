;; -*- lexical-binding: t; -*-

;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

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

;; This maps XTERMINAL/CSI escape sequences in the form
;; "CSI 27 ; mod ; base ~" to appropriate Emacs key representations See:
;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Extended-keyboard-protocol

;;; Code:

(defconst modify-other-keys--modifiers-alist
  '(("S-" . 2)
    ("M-" . 3)
    ("M-S-" . 4)
    ("C-" . 5)
    ("C-S-" . 6)
    ("C-M-" . 7)
    ("C-M-S-" . 8)))

(defun modify-other-keys--define-key (base key-string)
  (cl-loop for (prefix . mod) in modify-other-keys--modifiers-alist
           for xterm-seq = (format "\e[27;%d;%d~" mod base)
           for csi-u-seq = (format "\e[%d;%du" base mod)
           for emacs-key = (key-parse (concat prefix key-string))
           do
           (define-key input-decode-map xterm-seq emacs-key)
           (define-key input-decode-map csi-u-seq emacs-key)))

(defun modify-other-keys--setup-input-decode-map ()
  "Set up `input-decode-map' for the current terminal."
  ;; Printable characters
  (cl-loop for base from 33 upto 126
           do (modify-other-keys--define-key base (char-to-string base)))

  ;; Special whitespace characters
  (cl-loop for (key . base) in '(("DEL" . ?\d)
                                 ("RET" . ?\r)
                                 ("TAB" . ?\t)
                                 ("SPC" . ?\s)
                                 ("ESC" . ?\e))
           do (modify-other-keys--define-key base key)))

;; Translate S-<digit> combinations to their corresponding punctuation
;; symbols This allows C-S-1 to be interpreted as C-!, M-S-4 as M-$,
;; etc.
;;
;; Normal keyboards produce punctuation symbols when pressing
;; S-<digit>: S-0 = ), S-1 = !, etc., but Emacs receives these as
;; separate shifted keys.  This translation maps them to what users
;; would expect when combining with other modifiers.
(cl-loop for (unshifted . shifted) in '(("S-0" . ")")
                                        ("S-1" . "!")
                                        ("S-2" . "@")
                                        ("S-3" . "#")
                                        ("S-4" . "$")
                                        ("S-5" . "%")
                                        ("S-6" . "^")
                                        ("S-7" . "&")
                                        ("S-8" . "*")
                                        ("S-9" . "("))
         do (cl-loop for mod in '("C-" "M-" "C-M-")
                     do (define-key key-translation-map
                                    (kbd (concat mod unshifted))
                                    (kbd (concat mod shifted)))))

(defun modify-other-keys--init (&optional terminal)
  (when (and (eq t (terminal-live-p terminal))
             (not (string= (terminal-name terminal) "initial_terminal")))
    (with-selected-frame (car (frames-on-display-list terminal))
      (modify-other-keys--setup-input-decode-map)
      ;; TODO: use xterm--query to query whether kkp is support
      (send-string-to-terminal "\e[>4;2m")
      ;; (send-string-to-terminal "\e[>4;1f")
      ;; (push "\e[>4f" (terminal-parameter terminal 'tty-mode-reset-strings))
      (push "\e[>4m" (terminal-parameter terminal 'tty-mode-reset-strings))
      (delete "\e[>4;1m" (terminal-parameter terminal 'tty-mode-set-strings))
      ;; (push "\e[>4;1f" (terminal-parameter terminal 'tty-mode-set-strings))
      (push "\e[>4;2m" (terminal-parameter terminal 'tty-mode-set-strings))
      (set-terminal-parameter terminal 'modify-other-keys-mode t))))

(add-hook 'terminal-init-xterm-hook 'modify-other-keys--init)
(mapc 'modify-other-keys--init (terminal-list))

;; Provide the feature
(provide 'modify-other-keys)
;;; modify-other-keys.el ends here
