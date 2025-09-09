;; -*- lexical-binding: t; -*-

(declare-function straight-use-package "straight.el")

(setq straight-host-usernames
      '((github . "fuzy112")
	(codeberg . "fuzy")))

;; Prefer built-in packages
(straight-use-package '(eglot :type built-in))
(straight-use-package '(eldoc :type built-in))
(straight-use-package '(erc :type built-in))
(straight-use-package '(external-completion :type built-in))
(straight-use-package '(flymake :type built-in))
(straight-use-package '(map :type built-in))
(straight-use-package '(modus-themes :type built-in))
(straight-use-package '(org :type built-in))
(straight-use-package '(peg :type built-in))
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))
(straight-use-package '(editorconfig :type built-in))
(straight-use-package '(jsonrpc :type built-in))

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
(straight-use-package 'timeout)

;;; Terminal integration
(straight-use-package 'kkp)
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

;;;; Copilot
(straight-use-package 'copilot)

;;;; LSP
(straight-use-package 'eglot-tempel)
(straight-use-package 'breadcrumb)

;;;; Geiser
(straight-use-package 'geiser)
(straight-use-package 'geiser-chez)
(straight-use-package 'geiser-chicken)
(straight-use-package 'geiser-guile)

;;;; SLY
(straight-use-package 'sly)

;;;; Inf-clojure
(straight-use-package 'inf-clojure)

;;;; Inf-ruby
(straight-use-package 'inf-ruby)

;;;; Code formatting

(straight-use-package 'apheleia)

;;; Search

(straight-use-package 'rg)
(straight-use-package '(p-search :host github :repo "zkry/p-search" :files (:defaults "extensions")))


;;; UI

;;;; Nerd-icons

(straight-use-package 'nerd-icons)
(straight-use-package 'nerd-icons-completion)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'nerd-icons-ibuffer)
(straight-use-package '(nerd-icons-multimodal :host github :repo "abougouffa/nerd-icons-multimodal"))
(straight-use-package '(nerd-icons-grep :host github :repo "hron/nerd-icons-grep"))

;;;; Themes
(straight-use-package 'modus-themes)


;;;; Mode line
(straight-use-package 'blackout)

;;;; Font-lock
(straight-use-package 'outline-minor-faces)
(straight-use-package 'paren-face)
(straight-use-package 'hl-todo)

;;;; Visual aids
(straight-use-package 'goggles)
(straight-use-package 'pulsar)
(straight-use-package 'lin)
(straight-use-package 'prism)

;;;; Scrolling
(straight-use-package '(image-slicing :host github :repo "ginqi7/image-slicing"))
(straight-use-package '(ultra-scroll :host github :repo "jdtsmith/ultra-scroll"))


;;;; Colorful mode

(straight-use-package 'colorful-mode)

;;; Buffer management
(straight-use-package 'buffer-env)
(straight-use-package '(ws-butler :host github :repo "lewang/ws-butler"
				  :branch "master"))

;;; Version control
(straight-use-package 'magit)
(straight-use-package 'git-modes)
(straight-use-package 'forge)
(straight-use-package 'diff-hl)

;;; eldoc
(straight-use-package 'eldoc-box)
(straight-use-package '(eldoc-diffstat :host github :repo "kljohann/eldoc-diffstat"))

;;; Input methods
(straight-use-package 'pyim)
(straight-use-package 'pyim-basedict)
(straight-use-package '(rime :fork (:host github :repo "fuzy112/emacs-rime")))

;;; AI/LLM
(straight-use-package '(gptel :fork (:host github :repo "fuzy112/gptel")
			      :build (:not compile)))
(straight-use-package 'gptel-aibo)
(straight-use-package '(claude-code-ide :host github :repo "manzaltu/claude-code-ide.el"))
(straight-use-package '(eca :host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
(straight-use-package '(whisper :host github :repo "natrys/whisper.el"))

;;; Modal editing
(straight-use-package 'meow)
(straight-use-package 'meow-tree-sitter)
(straight-use-package '(repeat-fu :host codeberg :repo "ideasman42/emacs-repeat-fu"))

;;; Help and documentation

(straight-use-package 'devdocs)


;;; Undo
(straight-use-package 'vundo)

;;; Org
(straight-use-package 'org-modern)

;;; TeX

(straight-use-package 'auctex)

;;; Editing

(straight-use-package 'valign)
(straight-use-package 'ftable)
(straight-use-package 'olivetti)
(straight-use-package 'adaptive-wrap)
(straight-use-package 'visual-fill-column)
(straight-use-package 'logos)
(straight-use-package 'paredit)

;;; Extra major modes
(straight-use-package 'clojure-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'ruby-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'go-mode)
(straight-use-package 'systemd)
(straight-use-package 'toml-mode)
(straight-use-package 'vala-mode)
(straight-use-package 'meson-mode)
(straight-use-package '(cmake-mode :host github :repo "emacsmirror/cmake-mode" :files ("*.el")))
(straight-use-package 'markdown-mode)
(straight-use-package 'debian-el)
(straight-use-package 'dpkg-dev-el)
(straight-use-package 'pdf-tools)
(straight-use-package '(pcap-mode :fork (:host github :repo "fuzy112/pcap-mode")))

;;; Applications
(straight-use-package 'telega)
(straight-use-package 'ement)
(straight-use-package 'show-font)
(straight-use-package 'elfeed)
(straight-use-package '(eat :files (:defaults "integration" "term" "terminfo")
			    :fork (:host codeberg :repo "fuzy/emacs-eat")))
(straight-use-package '(video-trimmer :host github :repo "xenodium/video-trimmer"
				      :fork (:host github :repo "fuzy112/video-trimmer")))
(straight-use-package '(whisper :host github :repo "natrys/whisper.el"))

;;; Email
(straight-use-package 'autocrypt)
