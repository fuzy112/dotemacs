;;; dotemacs-bindings.el  -*- lexical-binding: t; -*-

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


;;;; keybindings

(defalias 'window-prefix-map window-prefix-map)
(defalias 'project-prefix-map project-prefix-map)

(define-keymap :keymap project-prefix-map
  "m" #'magit-project-status
  "=" #'+project/vc-diff
  "b" #'consult-project-buffer ; orig. project-switch-to-buffer
  "C-b" nil ; orig. `project-list-buffers'
  "t" #'eat-project
  "g" #'consult-ripgrep)

(define-keymap :keymap vc-prefix-map
  "." #'+vc/dir-here)

;; [[https://karthinks.com/software/even-more-batteries-included-with-emacs/#the-apropos-family][Even More Batteries Included with Emacs | Karthinks]]
(defvar-keymap help-apropos-map
  :prefix 'help-apropos-map
  :doc "apropos 子命令的按键映射。"
  "a"   #'apropos
  "l"   #'apropos-library
  "f"   #'apropos-function
  "x"   #'apropos-command
  "v"   #'apropos-variable
  "V"   #'apropos-local-variable
  "u"   #'apropos-user-option
  "d"   #'apropos-documentation
  "C-f" #'customize-apropos-faces
  "g"   #'customize-apropos-groups
  "o"   #'customize-apropos-options
  "c"   #'customize-apropos
  "i"   #'info-apropos)

(define-keymap :keymap help-map
  "a" #'help-apropos-map
  "H" #'+mail-to-help-gnu-emacs
  "G" #'+gnus-read-ephemeral-emacs-search-group
  "B" #'embark-bindings
  "I" #'consult-info
  "M" #'consult-man)

(defvar-keymap tool-map
  :doc    "Keymap for calling external tools."
  :prefix 'tool-map
  "A"     #'gptel
  "a"     #'gptel-menu
  "t"     #'tui-run
  "r"     #'tui-rg
  "R"     #'tui-recentf
  "g"     #'tui-ugrep
  "G"     #'tui-git-ls-files
  "y"     #'tui-yazi
  "k"     #'tui-kill
  "l"     #'tui-line
  "f"     #'tui-find
  "d"     #'tui-locate
  "p"     #'tui-switch-project
  "e"     #'eshell
  "s"     #'eat)

(defvar-keymap doc-map
  :doc    "Documentation commands."
  :prefix 'doc-map
  "d"     #'devdocs-lookup
  "i"     #'devdocs-install
  "p"     #'devdocs-peruse
  "r"     #'rust-docs-lookup
  "g"     #'good-doc-lookup)

(defvar-keymap file-map
  :doc    "Open file commands."
  :prefix 'file-map
  "e"     #'find-user-emacs-file
  "f"     #'find-file
  "i"     #'find-user-init-file
  "a"     #'ffap
  "r"     #'ff-find-related-file
  "R"     #'consult-recent-file
  "C-r"   #'recentf
  "n"     #'rename-visited-file
  "b"     #'backup-list-backups)

(defvar-keymap toggle-map
  :doc    "Keymap for toggling options."
  :prefix 'toggle-map
  "e"     #'eglot
  "f"     #'flymake-mode
  "q"     #'display-fill-column-indicator-mode
  "l"     #'display-line-numbers-mode
  "o"     #'outline-minor-mode
  "c"     #'+visual-fill-column/toggle-visual-fill-and-center
  "C"     #'olivetti-mode
  "x"     #'+toggle-transparent
  "v"     #'visual-line-mode
  "w"     #'whitespace-mode
  "t"     #'consult-theme
  "T"     #'reload-enabled-themes)

(defvar-keymap debug-map
  :doc    "Keymap for debugging commands."
  :prefix 'debug-map
  "e"     #'toggle-debug-on-error
  "q"     #'toggle-debug-on-quit
  "f"     #'debug-on-entry
  "v"     #'debug-on-variable-change
  "c f"   #'cancel-debug-on-entry
  "c v"   #'cancel-debug-on-variable-change)

(defvar-keymap quilt-prefix-map
  :prefix 'quilt-prefix-map
  "n" #'quilt-new-patch
  "a" #'quilt-add-visited-file
  "-" #'quilt-pop
  "+" #'quilt-push
  "f" #'quilt-list-files
  "g" #'quilt-refresh
  "l" #'quilt-list-applied-patches)

(define-keymap :keymap mode-specific-map
  "A"   #'org-agenda
  "C"   #'org-capture
  "D"   debug-map
  "L"   #'org-store-link
  "T"   #'telega
  "G"   #'gnus
  "V"   #'vundo
  "a"   #'embark-act
  "b"   #'consult-buffer
  "t"   tool-map
  "d"   doc-map
  "e"   #'cape-prefix-map
  "f"   file-map
  "!"   #'bangs
  "o"   toggle-map
  "h"   #'consult-history
  "M-g" #'magit-file-dispatch
  "p"   #'project-prefix-map
  "q"   quilt-prefix-map
  "r"   ctl-x-r-map
  "x"   ctl-x-x-map
  "R"   #'rg-menu
  "s"   search-map
  "v"   #'vc-prefix-map
  "w"   #'window-prefix-map

  "M-x" #'consult-mode-command
  "H"   #'consult-history
  "k"   #'consult-kmacro

  "C-/" #'apheleia-format-buffer
  "C-k" #'compile
  "C-p" #'send-password-to-process
  "C-l" #'org-insert-link-global)

(define-keymap :keymap ctl-x-4-map
  "t" #'eat-other-window
  "e" #'+eshell/other-window
  "b" #'consult-buffer-other-window
  "F" #'find-function-other-window
  "V" #'find-variable-other-window
  "K" #'find-function-on-key-other-window
  "L" #'find-library-other-window
  "g" #'magit-status-other-window)

(define-keymap :keymap ctl-x-5-map
  "F" #'find-function-other-frame
  "V" #'find-variable-other-frame
  "K" #'find-function-on-key-other-frame
  "L" #'find-library-other-frame
  "b" #'consult-buffer-other-frame)

(define-keymap :keymap ctl-x-r-map
  "b" #'consult-bookmark ; orig. bookmark-jump
  "u" #'url-bookmark-add)

(declare-function consult-register "ext:consult.el")

(define-keymap :keymap ctl-x-x-map
  "/" #'apheleia-format-buffer)

(define-keymap :keymap window-prefix-map
  "f 2" #'window-layout-flip-topdown
  "f 3" #'window-layout-flip-leftright
  "r r" #'window-layout-rotate-clockwise
  "r l" #'window-layout-rotate-anticlockwise
  "h"   #'+toggle-side-window-left
  "l"   #'+toggle-side-window-right
  "k"   #'+toggle-side-window-top
  "j"   #'+toggle-side-window-bottom)

(define-keymap :keymap tab-prefix-map
  "b" #'consult-buffer-other-tab ; orig. switch-to-buffer-other-tab
  )

(define-keymap :keymap ctl-x-map
  "F"   #'find-function
  "V"   #'find-variable
  "K"   #'find-function-on-key
  "L"   #'find-library
  "k"   #'smart-kill-buffer
  "g"   #'magit-status-quick
  "M-g" #'magit-dispatch
  "M-:" #'consult-complex-command ; orig. repeat-complex-command
  "C-d" #'consult-dir)

(define-keymap :keymap minibuffer-local-map
  "M-A"		#'marginalia-cycle
  "M-r"		#'consult-history
  "C-x C-d"	#'consult-dir
  "C-x C-j"	#'consult-dir-jump-file
  "C-c C-c"	#'embark-collect
  "C-c C-e"	#'embark-export)

(define-keymap :keymap isearch-mode-map
  "M-e"   #'consult-isearch-history
  "M-s e" #'consult-isearch-history
  "M-s l" #'consult-line
  "M-s L" #'consult-line-multi)

(define-keymap :keymap search-map
  "b" #'consult-browser-hist
  "d" #'consult-find
  "D" #'consult-fd
  "c" #'consult-locate
  "g" #'consult-grep
  "G" #'consult-git-grep
  "r" #'consult-ripgrep
  "R" #'consult-ugrep
  "l" #'consult-line
  "L" #'consult-line-multi
  "k" #'consult-keep-lines
  "u" #'consult-focus-lines
  "e" #'consult-isearch-history)

(define-keymap :keymap goto-map
  "e"     #'consult-compile-error
  "r"     #'consult-grep-match
  "f"     #'consult-flymake ; Alternative: consult-flycheck
  "g"     #'consult-goto-line ; orig. goto-line
  "M-g"   #'consult-goto-line ; orig. goto-line
  "o"     #'consult-outline ; Alternative: consult-org-heading
  "m"     #'consult-mark
  "k"     #'consult-global-mark
  "i"     #'consult-imenu
  "I"     #'consult-imenu-multi
  "t"     telega-prefix-map)

(define-keymap :keymap esc-map
  "o"		#'other-window
  ;; vertico
  "R"		#'vertico-repeat
  ;;
  "c"		#'capitalize-dwim
  "l"		#'downcase-dwim
  "u"		#'upcase-dwim
  "C-S-d"	#'copy-from-above-command
  "C-S-u"       #'up-list
  "<delete>"    #'kill-word
  ;; tempel
  "+"		#'tempel-complete
  "*"		#'tempel-insert
  ;; jinx
  "$"		#'jinx-correct
  "C-$"		#'jinx-languages
  ;; consult
  "#"		#'consult-register-load
  "'"		#'consult-register-store
  "C-#"		#'consult-register
  ;; embark
  "."		#'embark-dwim)

(define-keymap :keymap global-map
  "<f5>"			#'compile
  "C-S-d"			#'duplicate-dwim
  "C-z"				#'zap-up-to-char
  "<Copy>"                      #'kill-ring-save
  "<Cut>"			#'kill-region
  "<Paste>"                     #'yank
  ;; embark
  "C-."				#'embark-act
  "C-;"				#'embark-dwim
  ;; windmove
  "S-<left>"			#'windmove-left
  "S-<right>"			#'windmove-right
  "S-<up>"			#'windmove-up
  "S-<down>"			#'windmove-down
  ;; remap some commands
  "<remap> <yank-pop>"          #'consult-yank-pop
  "<remap> <eval-last-sexp>"	#'pp-eval-last-sexp
  "<remap> <eval-expression>"	#'pp-eval-expression
  "<remap> <Info-search>"	#'consult-info
  "<remap> <list-buffers>"	#'ibuffer-jump
  "<remap> <delete-char>"	#'delete-forward-char
  "<remap> <shell-command>"     #'shell-command+)

(provide 'dotemacs-bindings)
;;; dotemacs-bindings.el ends here
