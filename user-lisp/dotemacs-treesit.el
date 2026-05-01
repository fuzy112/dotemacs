;; dotemacs-treesit.el -- Configuration for treesit -*- lexical-binding: t; -*-

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


(setq treesit-language-source-alist
      '((awk        "https://github.com/Beaglefoot/tree-sitter-awk")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (bibtex     "https://github.com/latex-lsp/tree-sitter-bibtex")
        (blueprint  "https://github.com/huanie/tree-sitter-blueprint")
        (c          "https://github.com/tree-sitter/tree-sitter-c")
        (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (clojure    "https://github.com/sogaiu/tree-sitter-clojure")
        (cmake      "https://github.com/uyha/tree-sitter-cmake")
        (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (dart       "https://github.com/ast-grep/tree-sitter-dart")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elixir     "https://github.com/elixir-lang/tree-sitter-elixir")
        (glsl       "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
        (heex       "https://github.com/phoenixframework/tree-sitter-heex")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (janet      "https://github.com/sogaiu/tree-sitter-janet-simple")
        (java       "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" :revision "master" :source-dir "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (julia      "https://github.com/tree-sitter/tree-sitter-julia")
        (kotlin     "https://github.com/fwcd/tree-sitter-kotlin")
        (latex      "https://github.com/latex-lsp/tree-sitter-latex")
        (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (magik      "https://github.com/krn-robin/tree-sitter-magik")
        (make       "https://github.com/tree-sitter-grammars/tree-sitter-make")
        (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown" :source-dir "tree-sitter-markdown/src")
        (markdown-inline   "https://github.com/tree-sitter-grammars/tree-sitter-markdown" :source-dir "tree-sitter-markdown-inline/src")
        (nix        "https://github.com/nix-community/tree-sitter-nix")
        (nu         "https://github.com/nushell/tree-sitter-nu")
        (org        "https://github.com/milisims/tree-sitter-org")
        (perl       "https://github.com/ganezdragon/tree-sitter-perl")
        (proto      "https://github.com/mitchellh/tree-sitter-proto")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (r          "https://github.com/r-lib/tree-sitter-r")
        (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust")
        (scala      "https://github.com/tree-sitter/tree-sitter-scala")
        (sql        "https://github.com/DerekStride/tree-sitter-sql" :revision "gh-pages")
        (surface    "https://github.com/connorlay/tree-sitter-surface")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" :revision "master" :source-dir "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" :revision "master" :source-dir "typescript/src")
        (typst      "https://github.com/uben0/tree-sitter-typst" :revision "master" :source-dir "src")
        (verilog    "https://github.com/gmlarumbe/tree-sitter-verilog")
        (vhdl       "https://github.com/alemuller/tree-sitter-vhdl")
        (vue        "https://github.com/tree-sitter-grammars/tree-sitter-vue")
        (wast       "https://github.com/wasm-lsp/tree-sitter-wasm" :source-dir "wast/src")
        (wat        "https://github.com/wasm-lsp/tree-sitter-wasm" :source-dir "wat/src")
        (wgsl       "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

(provide 'dotemacs-treesit)
;;; dotemacs-treesit.el ends here
