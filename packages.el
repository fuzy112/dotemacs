;; -*- lexical-binding: t; -*-

(require 'straight)
(add-to-list 'straight-built-in-pseudo-packages 'image)

(straight-use-package '(cmake-mode :host github :repo "emacsmirror/cmake-mode" :files ("*.el")))
(straight-use-package '(consult-everything :host github :repo "jthaman/consult-everything"))
(straight-use-package '(gptel-quick :host github :repo "karthinks/gptel-quick"))
(straight-use-package '(nerd-icons-multimodal :host github :repo "abougouffa/nerd-icons-multimodal"))
(straight-use-package '(term-keys :host github :repo "CyberShadow/term-keys"))
(straight-use-package 'anaphora)
(straight-use-package 'apheleia)
(straight-use-package 'adaptive-wrap)
(straight-use-package 'breadcrumb)
(straight-use-package 'browser-hist)
(straight-use-package 'buffer-env)
(straight-use-package 'cape)
(straight-use-package 'clipetty)
(straight-use-package 'colorful-mode)
(straight-use-package 'consult)
(straight-use-package 'corfu)
(straight-use-package 'corfu-terminal)
(straight-use-package 'dash)
(straight-use-package 'deadgrep)
(straight-use-package 'debian-el)
(straight-use-package 'devdocs)
(straight-use-package 'diff-hl)
(straight-use-package 'doom-modeline)
(straight-use-package 'eglot)
(straight-use-package 'eglot-tempel)
(straight-use-package 'eldoc)
(straight-use-package 'eldoc-diffstat)
(straight-use-package 'embark-consult)
(straight-use-package 'erc)
(straight-use-package 'f)
(straight-use-package 'geiser)
(straight-use-package 'geiser-chicken)
(straight-use-package 'goggles)
(straight-use-package 'gptel)
(straight-use-package 'hl-todo)
(straight-use-package 'lin)
(straight-use-package 'logos)
(straight-use-package 'magit)
(straight-use-package 'marginalia)
(straight-use-package 'markdown-mode)
(straight-use-package 'meow-tree-sitter)
(straight-use-package 'modus-themes)
(straight-use-package 'nerd-icons)
(straight-use-package 'nerd-icons-completion)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'nerd-icons-ibuffer)
(straight-use-package 'orderless)
(straight-use-package 'org)
(straight-use-package 'outline-minor-faces)
(straight-use-package 'paren-face)
(straight-use-package 'pass)
(straight-use-package 'popper)
(straight-use-package 'posframe)
(straight-use-package 'pulsar)
(straight-use-package 'puni)
(straight-use-package 'pyim)
(straight-use-package 'rime)
(straight-use-package 'ruby-mode)
(straight-use-package 's)
(straight-use-package 'transducers)
(straight-use-package 'vala-mode)
(straight-use-package 'vertico)
(straight-use-package 'ws-butler)
(straight-use-package 'xterm-color)
(straight-use-package 'yaml-mode)
(straight-use-package `(eat :files ("*.el" "*.texi" "*.info" "integration" "term" "terminfo")))
(straight-use-package '(discourse :host codeberg :repo "glenneth/discourse-emacs"))
