;;; casual-config.el --- Configuration for Casual -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

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

(eval-when-compile
  (require 'dotemacs-core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Casual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org-Agenda
(after-load! org-agenda
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  (keymap-set org-agenda-mode-map "M-j" #'org-agenda-clock-goto)
  (keymap-set org-agenda-mode-map "J" #'bookmark-jump))

;; Org

(after-load! org
  (keymap-set org-mode-map "M-m" #'casual-org-tmenu)
  (keymap-set org-table-fedit-map "M-m" #'casual-org-table-fedit-tmenu))

;; Bookmarks

(after-load! bookmark
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  (keymap-set bookmark-bmenu-mode-map "J" #'bookmark-jump))

(after-load! casual-bookmarks
  (easy-menu-add-item global-map '(menu-bar)
		      casual-bookmarks-main-menu
		      "Tools"))

(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode) ; optional

;; Calc

(after-load! calc
  (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu))
(after-load! calc-alg
  (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu))

;; Dired

(after-load! dired
  (require 'dired-x)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu)
  (keymap-set dired-mode-map "M-s" #'casual-dired-search-replace-tmenu))

;; Ibuffer

(after-load! ibuffer
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu))

;; Info

(after-load! info
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu))

;; Isearch

(after-load! isearch
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu))

;; Re-builder

(after-load! re-builder
  (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu))

;; Eshell

(after-load! eshell
  (keymap-set eshell-mode-map "C-o" #'casual-eshell-tmenu))

;; Eww

(after-load! eww
  (keymap-set eww-mode-map "C-o" #'casual-eww-tmenu))

;; Compile

(after-load! compile
  (keymap-set compilation-mode-map "C-o" #'casual-compile-tmenu))

(after-load! grep
  (keymap-set grep-mode-map "C-o" #'casual-compile-tmenu))

;; EditKit (global bindings)

(keymap-global-set "C-c C-o" #'casual-editkit-main-tmenu)

(provide 'casual-config)
;;; casual-config.el ends here
