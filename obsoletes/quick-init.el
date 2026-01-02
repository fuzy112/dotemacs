;;; -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright © 2024, 2025  Zhengyi Fu <i@fuzy.me>

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

;;

;;; Code:

(require-theme 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t)
(modus-themes-load-vivendi)

(setq completion-styles '(flex basic))
(setq tab-always-indent 'complete)

(setq version-control t
      kept-new-versions 9
      kept-old-versions 9
      delete-old-versions t)

(recentf-mode)
(save-place-mode)
(let ((inhibit-message t))
  (repeat-mode))
(electric-pair-mode)

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-jump)
(keymap-global-set "M-o" #'other-window)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; indent-tabs-mode: t
;; End:
