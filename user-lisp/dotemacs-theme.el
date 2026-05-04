;;; dotemacs-theme.el  -*- lexical-binding: t; -*-

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


(eval-when-compile (require 'dotemacs-core)
                   (require 'llama))

;;;; fonts

;; The following fonts need to be installed:
;;  - https://github.com/be5invis/Iosevka/releases/download/v31.8.0/SuperTTC-SGr-IosevkaSS04-31.8.0.zip
;;  - https://github.com/be5invis/Sarasa-Gothic/releases/download/v1.0.26/Sarasa-SuperTTC-1.0.26.7z
;;  - https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/NerdFontsSymbolsOnly.zip

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
  (let* ((nf-font-family "Symbols Nerd Font")
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

;;;; themes

;; Any Elisp file can run arbitrary code, so there is no reason to
;; handle themes differently.
(setopt custom-safe-themes t)

(defun +inhibit-implied-resize (&rest args)
  "Prevent implied resize when switching themes."
  (let ((frame-inhibit-implied-resize t))
    (apply args)))

(advice-add 'enable-theme :around #'+inhibit-implied-resize)
(advice-add 'disable-theme :around #'+inhibit-implied-resize)

;;;; modus-theme

(after-load! modus-themes
  (setopt modus-themes-mixed-fonts        t
          modus-themes-bold-constructs    t
          modus-themes-slanted-constructs t
          modus-themes-variable-pitch-ui  nil
          modus-themes-to-toggle          '(modus-vivendi modus-operandi))
  (setopt modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (load-theme 'modus-operandi t t)
  (load-theme 'modus-vivendi t t))

(after-init!
  (when (not custom-enabled-themes)
    (require 'modus-themes)
    (load-theme 'modus-operandi t)))

;; Define a dedicated theme `dotemacs' instead of using the default
;; `user' theme to prevent the Emacs customization system from
;; automatically persisting face customizations.
(deftheme dotemacs
  "Central theme for customizing fonts and face properties in this configuration.")

;; Force face settings in this theme to take immediate effect.
(put 'dotemacs 'theme-immediate t)

;; Activate the custom theme immediately after definition
(enable-theme 'dotemacs)

;; Remove `dotemacs' from the list of enabled custom themes to prevent
;; it from being disable when using commands like `consult-theme'.
(delq! 'dotemacs custom-enabled-themes)

(defun dotemacs-theme-refresh (&optional _theme)
  "Customize and set faces for the dotemacs theme.
This function refreshes the dotemacs theme by setting various face
attributes."
  (interactive)
  ;; Temporarily bind `custom--inhibit-theme-enable' to nil to ensure
  ;; the face customizations take effect immediately.
  (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     'dotemacs
     `(default
       ((((type tty) (class color) (background dark))
         :background ,(if-let* ((bg (face-attribute 'default :background))
                                (colors (color-values bg))
                                ((equal '(0 0 0) colors)))
                          "unspecified-bg"
                        bg))))
     ;; `(cursor
     ;;   ((((class color) (min-colors 256) (background light)) :background "#005077")
     ;;    (((class color) (min-colors 256) (background dark)) :background "#40c8ec")))
     ;; `(meow-insert-cursor
     ;;   ((((class color) (min-colors 256) (background dark)) :background "#ec7745")))
     ;; `(meow-normal-cursor
     ;;   ((((class color) (min-colors 256) (background light)) :background "#005077")
     ;;    (((class color) (min-colors 256) (background dark)) :background "#40c8ec")))
     ;; `(meow-motion-cursor
     ;;   ((((class color) (min-colors 256) (background light)) :background "#005077")
     ;;    (((class color) (min-colors 256) (background dark)) :background "#40c8ec")))
     `(fill-column-indicator
       ((((type w32 tty))
         :height 1.0 :foreground "gray50" :background ,(face-background 'default))))
     `(diff-hl-margin-insert
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-insert)))
     `(diff-hl-margin-delete
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-delete)))
     `(diff-hl-margin-change
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-change)))
     `(diff-hl-margin-ignored
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-ignored)))
     `(diff-hl-margin-unknown
       ((t :family ,(face-attribute 'default :family)
           :foreground ,(face-foreground 'default)
           :slant normal :weight regular
           :inherit diff-hl-unknown)))
     '(whitespace-indentation ((t :underline "yellow")))
     '(whitespace-space-before-tab ((t :underline "DarkOrange")))
     '(whitespace-space-after-tab ((t :underline "yellow")))
     '(parenthesis
       ((t :inherit shadow)))
     ;; `(header-line
     ;;   ((((supports :underline t) (class color grayscale))
     ;;     :background ,(face-background 'default)
     ;;     :underline ( :color ,(face-foreground 'default)
     ;;                  :style line
     ;;                  :position t)
     ;;     :box (:line-width 6 :style flat-button))))
     ;; `(header-line-inactive
     ;;   ((t :inherit (shadow header-line))))
     ;; `(mode-line-active
     ;;   ((((supports :overline t) (class color grayscale))
     ;;     :background ,(face-background 'default)
     ;;     :foreground ,(face-foreground 'default)
     ;;     :overline ,(face-foreground 'default)
     ;;     :box ( :line-width 6
     ;;            :color ,(face-background 'default)
     ;;            :style nil))))
     ;; `(mode-line-inactive
     ;;   ((((supports :overline t) (class color grayscale))
     ;;     :background ,(face-background 'default)
     ;;     :foreground ,(face-foreground 'shadow)
     ;;     :overline t
     ;;     :box ( :line-width 6
     ;;            :color ,(face-background 'default)
     ;;            :style nil))))

     ;; `(tab-line-tab-current
     ;;   ((((min-colors 256) (background dark))
     ;;     :background "purple"
     ;;     :foreground "white"
     ;;     :box nil)))
     ;; `(tab-line-tab-inactive
     ;;   ((((min-colors 256) (background dark))
     ;;     :background "DarkGreen"
     ;;     :foreground "white"
     ;;     :box nil)))

     ;; `(tab-bar
     ;;   ((((supports :box t))
     ;;     :box ( :line-width (-2 . 6)
     ;;            :style flat-button))))
     '(region
       ((t :foreground unspecified)))
     '(secondary-selection
       ((t :foreground unspecified)))
     '(minibuffer-depth-indicator
       ((t :inherit shadow)))
     ;; FIXME It seems that setting :fontset here doesn't work at all.
     ;; term
     '(term
       ((t :family "IosevkaTerm SS04" :fontset "fontset-term")))
     ;; eat
     '(eat-term-font-0
       ((t :family "IosevkaTerm SS04" :fontset "fontset-term"))))))

(defun dotemacs-theme-disable ()
  "Disable the dotemacs theme."
  (interactive)
  (push 'dotemacs custom-enabled-themes)
  (disable-theme 'dotemacs)
  (remove-hook 'server-after-make-frame-hook #'dotemacs-theme-refresh)
  (remove-hook 'enable-theme-functions #'dotemacs-theme-refresh))

(when (daemonp)
  ;; Refresh theme for new frames in server mode
  (add-hook 'server-after-make-frame-hook #'dotemacs-theme-refresh t))

(after-init!
  (dotemacs-theme-refresh))

;; Refresh theme when themes are enabled
(add-hook 'enable-theme-functions #'dotemacs-theme-refresh)

(defvar consult-theme--save-variable t)

(defun consult-theme--state ()
  (let ((saved-theme (car custom-enabled-themes)))
    (lambda (action theme)
      (with-selected-window (or (active-minibuffer-window)
                                (selected-window))
        (let (consult-theme--save-variable)
          (pcase action
            ('return (consult-theme (or theme saved-theme)))
            ((and 'preview (guard theme)) (consult-theme theme))))))))

(with-eval-after-load 'consult
  (with-no-compile!
    (consult-customize consult-theme :state (consult-theme--state))))

(define-advice consult-theme (:after (theme) save)
  "Advice to persist theme selections after using `consult-theme'.
Saves `custom-enabled-themes' to customize settings permanently, and adds
the selected theme's SHA256 hash to `custom-safe-themes' if the theme is not
already marked as safe and is not a built-in default Emacs theme."
  (when consult-theme--save-variable
    (let ((file (locate-file (concat (symbol-name theme) "-theme.el")
                             (custom-theme--load-path)
                             '("" "c"))))
      (unless (or (not file)
                  (eq custom-safe-themes t)
                  (and (memq 'default custom-safe-themes)
                       (equal (file-name-directory file)
                              (expand-file-name "themes/" data-directory))))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((hash (secure-hash 'sha256 (current-buffer))))
            (unless (member hash custom-safe-themes)
              (customize-save-variable 'custom-safe-themes
                                       (cons hash custom-safe-themes))))))
      (customize-save-variable 'custom-enabled-themes custom-enabled-themes
                               "Saved by `consult-theme'."))))

(defun reload-enabled-themes ()
  (interactive)
  (mapc (##load-theme %1 t t)
        custom-enabled-themes))

(provide 'dotemacs-theme)
;;; dotemacs-theme.el ends here
