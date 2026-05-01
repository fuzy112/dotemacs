;;; dotemacs-lsp.el  -*- lexical-binding: t; -*-

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

;;;; eglot

(after-load! eglot
  (define-advice eglot-completion-at-point (:around (fn &rest _args) tempel)
    (cape-wrap-super fn :with #'tempel-complete))
  (setopt eglot-autoshutdown t
          eglot-extend-to-xref t))

(after-load! eglot
  (defvar-keymap +eglot-prefix-map
    :prefix '+eglot-prefix-map
    "d" #'eglot-find-declaration
    "t" #'eglot-find-typeDefinition
    "i" #'eglot-find-implementation
    "s" #'consult-eglot-symbols
    "a" #'eglot-code-actions
    "n" #'eglot-code-action-inline
    "e" #'eglot-code-action-extract
    "r" #'eglot-code-action-rewrite
    "f" #'eglot-code-action-quickfix
    "o" #'eglot-code-action-organize-imports
    "/" #'eglot-format
    "c" #'eglot-show-call-hierarchy
    "T" #'eglot-show-type-hierarchy
    "w" #'eglot-show-workspace-configuration
    "C" #'eglot-signal-didChangeConfiguration
    "u" #'eglot-shutdown
    "U" #'eglot-shutdown-all)
  (keymap-set eglot-mode-map "C-x x /" #'eglot-format)
  (keymap-set eglot-mode-map "C-c C-a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c l" '+eglot-prefix-map)
  (eglot-tempel-mode))

;;;;; Nix integration

(defvar lsp-server-nix-packages
  '(("rust-analyzer"                   . ("nixpkgs#rust-analyzer" "nixpkgs#cargo"))
    ("neocmakelsp"                     . ("nixpkgs#neocmakelsp"))
    ("cmake-language-server"           . ("nixpkgs#cmake-language-server"))
    ("vim-language-server"             . ("nixpkgs#vim-language-server"))
    ("pyright-langserver"              . ("nixpkgs#pyright"))
    ("basedpyright-langserver"         . ("nixpkgs#basedpyright"))
    ("pyrefly"                         . ("nixpkgs#pyrefly"))
    ("ty"                              . ("nixpkgs#ty"))
    ("ruff"                            . ("nixpkgs#ruff"))
    ("vscode-json-language-server"     . ("nixpkgs#vscode-json-languageserver"))
    ("typescript-language-server"      . ("nixpkgs#typescript-language-server"))
    ("bash-language-server"            . ("nixpkgs#bash-language-server"))
    ("clangd"                          . ("nixpkgs#clang-tools" "nixpkgs#gcc"))
    ("solargraph"                      . ("nixpkgs#solargraph"))
    ("ruby-lsp"                        . ("nixpkgs#ruby-lsp"))
    ("haskell-language-server-wrapper" . ("nixpkgs#haskell-language-server"))
    ("kotlin-language-server"          . ("nixpkgs#kotlin-language-server"))
    ("gopls"                           . ("nixpkgs#gopls" "nixpkgs#go"))
    ("jdtls"                           . ("nixpkgs#jdt-language-server"))
    ("texlab"                          . ("nixpkgs#texlab"))
    ("erlang_ls"                       . ("nixpkgs#erlang-ls"))
    ("elp"                             . ("nixpkgs#erlang-language-platform"))
    ("yaml-language-server"            . ("nixpkgs#yaml-language-server"))
    ("tombi"                           . ("nixpkgs#tombi"))
    ("nixd"                            . ("nixpkgs#nixd"))
    ("nls"                             . ("nixpkgs#nls"))))

(defun eglot--executable-find@nix (fn command &optional remote)
  "Find executable COMMAND, possibly via Nix.
If COMMAND is not found in PATH and we are not in a remote buffer, look
up COMMAND in `lsp-server-nix-packages'.  If found, run `nix shell' with
the associated packages to locate the command.  If that succeeds and the
resulting program exists, return its path.  Otherwise, fall back to the
original function FN."
  (if-let*
      (((not (file-remote-p default-directory)))
       ((not (executable-find command remote)))
       (pkgs (cdr (assoc-string command lsp-server-nix-packages)))
       (prog
        (with-temp-buffer
          (apply #'call-process "nix" nil t nil "shell" "--quiet"
                 (append pkgs (list "-c" "which" command)))
          (string-trim (buffer-string))))
       ((file-exists-p prog)))
      prog
    (funcall fn command remote)))

(defun eglot--guess-contact@nix (result)
  "Adjust Eglot server contact to run via Nix if needed.
When RESULT contains a contact list and its command is not in PATH, look
up the command in `lsp-server-nix-packages'.  If found, wrap the contact
list to run via `nix shell' with the associated packages."
  (when-let*
      ((contact (nth 3 result))
       ((listp contact))
       (command (car (ensure-list contact)))
       ((not (executable-find command)))
       (pkgs (cdr (assoc-string command lsp-server-nix-packages))))
    (setf (nth 3 result)
          (append (list "nix" "shell" "--quiet") pkgs (list "-c")
                  contact)))
  result)

(define-minor-mode eglot-nix-mode
  "Global minor mode for Eglot with Nix-managed language servers.
When enabled, this mode integrates Nix-managed language servers with Eglot,
handling server discovery and environment setup automatically.
Disabling the mode restores the default Eglot behavior."
  :global t
  :group 'nix
  (advice-remove 'eglot--executable-find #'eglot--executable-find@nix)
  (advice-remove 'eglot--guess-contact #'eglot--guess-contact@nix)
  (when eglot-nix-mode
    (advice-add 'eglot--executable-find :around
                #'eglot--executable-find@nix '((name . nix)))
    (advice-add 'eglot--guess-contact :filter-return
                #'eglot--guess-contact@nix '((name . nix)))))

;;;;; consult-eglot
(after-load! consult
  ;; Add a split style for consult-eglot-symbols, which splits the input by spaces,
  ;; where the first component is the input to the LSP.
  (add-to-list 'consult-async-split-styles-alist `(space :separator ?\s :function ,#'consult--split-separator)))

(after-load! eglot
  (keymap-set eglot-mode-map "M-s s" #'consult-eglot-symbols)
  (keymap-set eglot-mode-map "M-s M-s" #'consult-eglot-symbols)
  (keymap-set eglot-mode-map "<remap> <xref-find-apropos>" 'consult-eglot-symbols))

(defun +consult--async-wrap--split-space (async)
  "Wrap ASYNC function with a pipeline that splits input on spaces.
This is a modified version of `consult--async-wrap' that uses
`consult--async-split' with the `space' argument to split the
input stream on spaces instead of the default newline character."
  (consult--async-pipeline
   (consult--async-split 'space)
   async
   (consult--async-indicator)
   (consult--async-refresh)))

(after-load! (:and consult-eglot consult)
  (with-no-compile!
    (consult-customize consult-eglot-symbols
                       :async-wrap #'+consult--async-wrap--split-space)))

(defun +consult-eglot-symbols--highlight (input)
  "Return a highlighting function for consult-eglot-symbols.
INPUT is the orderless pattern string.  The returned function
takes a candidate string and highlights matches using a regexp
generated by `orderless-flex' from INPUT."
  (let ((re (rx-to-string (orderless-flex input) t)))
    (lambda (str)
      (consult--highlight-regexps (list re) t str))))

(define-advice consult-eglot-symbols (:around (fun) highlight)
  "Highlight consult-eglot-symbols matches using orderless-flex regexps.
This advice overrides `consult--async-highlight' to generate regexps
from input using `orderless-flex', ensuring that symbol matches are
highlighted according to the orderless matching style."
  (let* ((orig-highlight (symbol-function 'consult--async-highlight))
         (new-highlight (lambda (&optional highlight)
                          (funcall orig-highlight
                                   (or highlight
                                       #'+consult-eglot-symbols--highlight)))))
    (cl-letf (((symbol-function 'consult--async-highlight) new-highlight))
      (funcall fun))))

(provide 'dotemacs-lsp)
;;; dotemacs-lsp.el ends here
