;;; dotemacs-fonts.el --- Font configuration -*- lexical-binding: t -*-
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

;; This file sets up a comprehensive font configuration for Emacs.  It
;; defines several custom fontsets (variable, fixed, term) and
;; configures fallback fonts for CJK characters, emoji, Nerd Font
;; symbols, and miscellaneous Unicode ranges.  The following fonts are
;; required:
;;
;;   - Iosevka SS04 (medium weight) – used as the base proportional font
;;   - Iosevka Aile – variable-pitch face
;;   - Iosevka Fixed Slab (or Iosevka Term Slab) – fixed-pitch faces
;;   - Sarasa Gothic CL (semibold) – CJK fallback for each fontset
;;   - Noto Color Emoji – emoji support
;;   - Symbols Nerd Font Mono – Nerd Font glyphs
;;   - Unifont / Unifont Upper – ultimate fallback for missing characters
;;
;; Installation links:
;;   https://github.com/be5invis/Iosevka/releases/download/v31.8.0/SuperTTC-SGr-IosevkaSS04-31.8.0.zip
;;   https://github.com/be5invis/Sarasa-Gothic/releases/download/v1.0.26/Sarasa-SuperTTC-1.0.26.7z
;;   https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/NerdFontsSymbolsOnly.zip
;;
;; After installing the fonts, the configuration is applied automatically
;; on graphical frames via the `+maybe-init-fontset' function.

;;; Code:

(eval-when-compile
  (require 'dotemacs-core))

(defvar +fontsets-initialize-hook nil)

(defun +init-fontsets ()
  "Initialize the font configuration"
  ;; Font spec format:
  ;;
  ;; -MAKER-FAMILY-WEIGHT-SLANT-WIDTHTYPE-STYLE...
  ;; ...-PIXELS-HEIGHT-HORIZ-VERT-SPACING-WIDTH-REGISTRY-ENCODING

  ;; Reset the predefined fontsets
  (set-fontset-font "fontset-startup" 'unicode (font-spec :family "Iosevka SS04" :weight 'medium))
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "Iosevka SS04" :weight 'medium))

  (setq use-default-font-for-symbols nil)

  ;; Create custom fontsets
  (dolist (fs '("-*-Iosevka Aile-medium-normal-normal-*-*-*-*-*-*-*-fontset-variable,"
                "-*-Iosevka Fixed Slab-medium-normal-normal-*-*-*-*-*-*-*-fontset-fixed,"
                "-*-IosevkaTerm SS04-medium-normal-normal-*-*-*-*-*-*-*-fontset-term,"))
    (create-fontset-from-fontset-spec fs))

  ;; Customize fonts for CJK characters
  (dolist (script '(han cjk-misc kana hangul bopomofo))
    (set-fontset-font "fontset-default" script "-*-Sarasa Gothic CL-semibold-*" nil 'append)
    (set-fontset-font "fontset-variable" script "-*-Sarasa UI CL-semibold-*")
    (set-fontset-font "fontset-fixed" script "-*-Sarasa Fixed Slab CL-semibold-*")
    (set-fontset-font "fontset-term" script "-*-Sarasa Term CL-semibold-*"))

  ;; Emoji
  (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Color Emoji"))

  ;; Nerd Font symbols
  (let* ((nf-font-family "Symbols Nerd Font Mono")
         (nf-font-spec (font-spec :family nf-font-family)))
    ;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
    (dolist (range '((#x23fb  . #x23fe)    ; IEC Power Symbols
                     (#x2b58  . #x2b58)    ; IEC Power Symbols
                     (#x2500  . #x259f)    ; Box Drawing
                     (#x2630  . #x2630)    ; Powerline Extra Symbols
                     (#x2665  . #x2665)    ; Octicons
                     (#x26a1  . #x26a1)    ; Octicons
                     (#x276c  . #x2771)    ; Heavy Angle Brackets
                     (#xe000  . #xe00a)    ; Pomicons
                     (#xe0a0  . #xe0a3)    ; Powerline Symbols + Extra (merged)
                     (#xe0b0  . #xe0d7)    ; Powerline Symbols + Extra (merged)
                     (#xe200  . #xe2a9)    ; Font Awesome Extension
                     (#xe300  . #xe3e3)    ; Weather Icons
                     (#xe5fa  . #xe6b7)    ; Seti-UI + Custom
                     (#xe700  . #xe8ef)    ; Devicons
                     (#xea60  . #xec1e)    ; Codicons
                     (#xed00  . #xefce)    ; Font Awesome (part 1, includes gap for progress)
                     (#xee00  . #xee0b)    ; Progress indicators
                     (#xf000  . #xf2ff)    ; Font Awesome (part 2)
                     (#xf300  . #xf381)    ; Font Logos
                     (#xf400  . #xf533)    ; Octicons
                     (#xf500  . #xfd46)    ; Material Design Icons (part 1)
                     (#xf0001 . #xf1af0))) ; Material Design Icons (part 2, supplementary private use area)
      (set-fontset-font "fontset-default" range nf-font-spec nil 'prepend)
      (set-fontset-font "fontset-variable" range nf-font-spec nil 'prepend)
      (set-fontset-font "fontset-fixed" range nf-font-spec nil 'prepend)
      (set-fontset-font "fontset-term" range nf-font-spec nil 'prepend)))

  ;; Fallback fonts
  (set-fontset-font "fontset-default" 'unicode (font-spec :family "Unifont") nil 'append)
  (set-fontset-font "fontset-default" 'unicode-smp (font-spec :family "Unifont Upper") nil 'append)
  (set-fontset-font "fontset-default" 'unicode-sip (font-spec :family "Unifont Upper") nil 'append)
  (set-fontset-font "fontset-default" 'unicode-ssp (font-spec :family "Unifont Upper") nil 'append)

  ;; Assign fontsets to faces
  (set-face-font 'default "fontset-default" nil)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :weight 'medium :fontset "fontset-variable")
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Term Slab" :weight 'medium :fontset "fontset-term")
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Term Slab" :weight 'medium :fontset "fontset-term")

  (after-init!
    (run-hooks '+fontsets-initialize-hook)))

(defvar +fontsets-initialized nil)

(defun +maybe-init-fontset ()
  (and (not +fontsets-initialized)
       (display-graphic-p)
       (with-demoted-errors "Failed to setup fonts: %S"
         (+init-fontsets)
         (setq +fontsets-initialized t))))

(after-init!
  (catch 'quit
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (and (+maybe-init-fontset)
             (throw 'quit t))))))

(add-hook 'server-after-make-frame-hook #'+maybe-init-fontset)

(setopt xft-ignore-color-fonts nil
        face-ignored-fonts nil)

;;; _

(provide 'dotemacs-fonts)
;;; dotemacs-fonts.el ends here
