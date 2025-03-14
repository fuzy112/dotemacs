;; -*- lexical-binding: t; -*-

(declare-function straight-use-package "straight.el")

;;; Utility libraries
(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'anaphora)
(straight-use-package 'transducers)
(straight-use-package 'pcre2el)
(straight-use-package 'posframe)
(straight-use-package 'xterm-color)
(straight-use-package 'persist)

;;; Terminal integration
(straight-use-package '(term-keys :host github :repo "CyberShadow/term-keys"))
(straight-use-package 'clipetty)

;;; Minibuffer
(straight-use-package 'orderless)
(straight-use-package 'vertico)
(straight-use-package 'marginalia)


;;;; Consult
(straight-use-package 'consult)
(straight-use-package 'consult-dir)
(straight-use-package 'consult-eglot)
(straight-use-package '(consult-everything :host github :repo "jthaman/consult-everything"))
(straight-use-package 'browser-hist)

;;;; Embark
(straight-use-package 'embark)
(straight-use-package 'embark-consult)

;;; In-buffer completion
(straight-use-package 'corfu)
(straight-use-package 'corfu-terminal)
(straight-use-package 'cape)

;;; Development

;;;; Tags

(straight-use-package 'citre)

;;;; Copilot
(straight-use-package 'copilot)
(straight-use-package 'copilot-chat)

;;;; LSP
(straight-use-package 'eglot)
(straight-use-package 'eglot-tempel)
(straight-use-package 'breadcrumb)

;;;; Geiser
(straight-use-package 'geiser)
(straight-use-package 'geiser-chez)
(straight-use-package 'geiser-chicken)
(straight-use-package 'geiser-guile)

;;;; SLY
(straight-use-package 'sly)

;;;; Inf-ruby
(straight-use-package 'inf-ruby)

;;;; Code formatting

(straight-use-package 'apheleia)

;;; Search

(straight-use-package 'deadgrep)
(straight-use-package '(p-search :host github :repo "zkry/p-search" :files (:defaults "extensions")))


;;; UI

;;;; Nerd-icons

(straight-use-package 'nerd-icons)
(straight-use-package 'nerd-icons-completion)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'nerd-icons-ibuffer)
(straight-use-package '(nerd-icons-multimodal :host github :repo "abougouffa/nerd-icons-multimodal"))

;;;; Themes
(straight-use-package 'modus-themes)


;;;; Mode line
(straight-use-package 'doom-modeline)

;;;; Font-lock
(straight-use-package 'outline-minor-faces)
(straight-use-package 'paren-face)
(straight-use-package 'hl-todo)

;;;; Visual aids
(straight-use-package 'goggles)
(straight-use-package 'pulsar)
(straight-use-package 'lin)

;;;; Scrolling
(straight-use-package '(image-slicing :host github :repo "ginqi7/image-slicing"))
(straight-use-package '(ultra-scroll :host github :repo "jdtsmith/ultra-scroll"))


;;;; Colorful mode

(straight-use-package 'colorful-mode)

;;; Buffer management
(straight-use-package 'buffer-env)
(straight-use-package 'ws-butler)
(straight-use-package 'popper)
(straight-use-package 'activities)

;;; Version control
(straight-use-package 'magit)
(straight-use-package 'diff-hl)

;;; eldoc
(straight-use-package 'eldoc)
(straight-use-package 'eldoc-box)
(straight-use-package '(eldoc-diffstat :host github :repo "kljohann/eldoc-diffstat"))

;;; Input methods
(straight-use-package 'pyim)
(straight-use-package 'pyim-basedict)
(straight-use-package '(rime :fork (:host github :repo "fuzy112/emacs-rime")))

;;; LLM
(straight-use-package '(gptel :build (:not compile)))
(straight-use-package '(gptel-quick :host github :repo "karthink/gptel-quick"))


;;; Modal editing
(straight-use-package 'meow)
(straight-use-package 'meow-tree-sitter)
(straight-use-package '(repeat-fu :host codeberg :repo "ideasman42/emacs-repeat-fu"))

;;; Help and documentation

(straight-use-package 'devdocs)


;;; Undo
(straight-use-package 'vundo)

;;; Org
(straight-use-package 'org)
(straight-use-package 'org-modern)

;;; Editing

(straight-use-package 'valign)
(straight-use-package 'ftable)
(straight-use-package 'olivetti)
(straight-use-package 'adaptive-wrap)
(straight-use-package 'visual-fill-column)
(straight-use-package 'logos)
(straight-use-package 'puni)

;;; Extra major modes
(straight-use-package 'yaml-mode)
(straight-use-package 'ruby-mode)
(straight-use-package 'rust-mode)
(straight-use-package '(systemd-mode :files (:defaults "*.txt")))
(straight-use-package 'toml-mode)
(straight-use-package 'vala-mode)
(straight-use-package 'meson-mode)
(straight-use-package '(cmake-mode :host github :repo "emacsmirror/cmake-mode" :files ("*.el")))
(straight-use-package 'markdown-mode)
(straight-use-package 'debian-el)
(straight-use-package 'dpkg-dev-el)
(straight-use-package 'pdf-tools)

;;; Applications
(straight-use-package 'pass)
(straight-use-package 'erc)
(straight-use-package 'show-font)

(straight-use-package '(eat :files (:defaults "integration" "term" "terminfo")))

;;; EXWM
(straight-use-package '(xelb :host github :repo "emacs-exwm/xelb"))
(straight-use-package '(exwm :host github :repo "emacs-exwm/exwm"))
(straight-use-package '(exwm-firefox :host codeberg :repo "emacs-weirdware/exwm-firefox"))
(straight-use-package 'ednc)

;;; Email
(straight-use-package 'autocrypt)
