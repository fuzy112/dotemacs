;;; dotemacs-email.el  -*- lexical-binding: t; -*-

;; Copyright © 2024-2026  Zhengyi Fu <i@fuzy.me>

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


(eval-when-compile (require 'dotemacs-core))

;;;; email and gnus

(after-load! gnus
  (setopt gnus-verbose 5
          gnus-verbose-backends 5))
(setopt mail-user-agent 'gnus-user-agent)

(after-load! message
  (setopt message-mail-alias-type 'ecomplete))
(declare-function message-ecomplete-capf "message.el")
(defun +message-ecompletion-capf-setup ()
  (add-hook 'completion-at-point-functions #'message-ecomplete-capf nil t))
(add-hook 'message-mode-hook #'+message-ecompletion-capf-setup)

(after-load! mm-decode
  (setopt mm-discouraged-alternatives '("text/html" "text/richtext")))

(after-load! gnus-art
  (require 'gnus-diff))


;;;; telega

(after-load! telega
  (require 'telega-config))

;;;; ement

(after-load! ement
  (setopt ement-save-sessions t))

;;;; rcirc

(after-load! rcirc
  (rcirc-track-minor-mode))

;;;; erc

(defvar erc-modules)
(defvar erc-mode-map)
(after-load! erc
  ;; This enables displaying servers and channels in side windows,
  ;; which can be toggled by C-x w s.
  (setopt erc-modules
          (seq-union '(sasl nicks scrolltobottom track)
                     erc-modules))

  ;; insert a newline when I hit <RET> at the prompt, and prefer
  ;; something more deliberate for actually send messages.
  (keymap-unset erc-mode-map "RET")
  (keymap-set erc-mode-map "C-c C-c" #'erc-send-current-line)

  ;; protect me from accidentally sending excess lines.
  (setopt erc-inhibit-multiline-input t
          erc-send-whitespace-lines t
          erc-ask-about-multiline-input t)
  ;; scroll all windows to prompt when submitting input.
  (setopt erc-scrolltobottom-all t)

  ;; reconnect automatically using a fancy strategy.
  (setopt erc-server-reconnect-function
          #'erc-server-delayed-check-reconnect
          erc-server-reconnect-timeout 30)

  ;; show new buffers in the current window instead of a split.
  (setopt erc-interactive-display 'buffer)

  ;; prefer one message line without continuation indicators.
  (setopt erc-fill-function #'erc-fill-wrap
          erc-fill-static-center 18)
  (after-load! erc-fill
    (require 'erc-fill)
    (keymap-set erc-fill-wrap-mode-map "C-c =" #'erc-fill-wrap-nudge))

  ;; prevent JOINs and PARTs from lighting up the mode-line.
  (after-load! erc-track
    (setopt erc-track-faces-priority-list (remq 'erc-notice-face
                                                erc-track-faces-priority-list)))
  (setopt erc-track-priority-faces-only 'all))


(provide 'dotemacs-email)
;;; dotemacs-email.el ends here
