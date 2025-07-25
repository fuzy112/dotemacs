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


;;; Code:

;; Map extended function keys with various modifiers
;; This maps XTERMINAL/CSI escape sequences in the form "CSI base ; mod u" to appropriate Emacs key representations
;; See: https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Extended-keyboard-protocol
;; - base is the base key code (ASCII value 33-126, printable characters except space)
;; - mod is the modifier value:
;;   2 = Shift
;;   3 = Alt (Meta)
;;   4 = Alt+Shift
;;   5 = Ctrl
;;   6 = Ctrl+Shift
;;   7 = Ctrl+Alt
;;   8 = Ctrl+Alt+Shift
;;
;; This loop handles all standard printable ASCII characters (excluding space)
(cl-loop for base from 33 upto 126
         do (cl-loop for modifier-bit in '(?\S-\0 ?\M-\0 ?\M-\S-\0 ?\C-\0 ?\C-\S-\0 ?\C-\M-\0 ?\C-\M-\S-\0)
                     for modifier-code in '(2 3 4 5 6 7 8)
                     ;; Create the terminal escape sequence (e.g., "\e[65;5u" for Ctrl-A)
                     for escape-sequence = (format "\e[%d;%du" base modifier-code)
                     ;; Create the Emacs key representation with appropriate modifier
                     for emacs-key = (vector (+ modifier-bit base))
                     ;; Map the escape sequence to the Emacs key
                     do (define-key input-decode-map escape-sequence emacs-key)))

;; Handle special whitespace characters with modifiers:
;; - ?\d = DEL (ASCII 127)
;; - ?\t = TAB (ASCII 9)
;; - ?\s = SPC (ASCII 32)
;; - ?\r = RET (ASCII 13)
;; - ?\e = ESC (ASCII 27)
(cl-loop for base in '(?\d ?\t ?\s ?\r ?\e)
         do (cl-loop for modifier-bit in '(?\S-\0 ?\M-\0 ?\M-\S-\0 ?\C-\0 ?\C-\S-\0 ?\C-\M-\0 ?\C-\M-\S-\0)
                     for modifier-code in '(2 3 4 5 6 7 8)
                     ;; Create the terminal escape sequence
                     for escape-sequence = (format "\e[%d;%du" base modifier-code)
                     ;; Create the Emacs key representation with appropriate modifier
                     for emacs-key = (vector (+ modifier-bit base))
                     ;; Map the escape sequence to the Emacs key
                     do (define-key input-decode-map escape-sequence emacs-key)))

;; Translate shift+digit combinations to their corresponding punctuation symbols
;; This allows Ctrl+Shift+1 to be interpreted as Ctrl+!, Meta+Shift+4 as Meta+$, etc.
;;
;; Normal keyboards produce punctuation symbols when pressing Shift+digit:
;; Shift+0 = ), Shift+1 = !, etc., but Emacs receives these as separate shifted keys.
;; This translation maps them to what users would expect when combining with other modifiers.
(cl-loop for (unshifted . shifted) in '((?\S-0 . ?\))  ; digit 0 to )
                                        (?\S-1 . ?!)   ; digit 1 to !
                                        (?\S-2 . ?@)   ; digit 2 to @
                                        (?\S-3 . ?#)   ; digit 3 to #
                                        (?\S-4 . ?$)   ; digit 4 to $
                                        (?\S-5 . ?%)   ; digit 5 to %
                                        (?\S-6 . ?^)   ; digit 6 to ^
                                        (?\S-7 . ?&)   ; digit 7 to &
                                        (?\S-8 . ?*)   ; digit 8 to *
                                        (?\S-9 . ?\()) ; digit 9 to (
         ;; For each digit and its shifted symbol
         do (cl-loop for mod in '(?\C-\0 ?\M-\0 ?\C-\M-\0) ; Apply each modifier combination (Ctrl, Meta, Ctrl+Meta)
                     ;; Generate the keybindings with modifiers
                     do (define-key key-translation-map
                                    (vector (+ unshifted mod))
                                    (vector (+ shifted mod)))))

;; Provide the feature
(provide 'modify-other-keys)
;;; modify-other-keys.el ends here
