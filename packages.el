;; -*- lexical-binding: t; -*-

(declare-function straight-use-package "straight.el")

(setq straight-host-usernames
      '((github . "fuzy112")
	(codeberg . "fuzy")))

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

(straight-use-package 'ctrlf)		; isearch is laggy when breadcrumb-mode is enabled
(straight-use-package 'rg)
(straight-use-package '(p-search :host github :repo "zkry/p-search" :files (:defaults "extensions")))

;;; UI

;;;; Nerd-icons

(straight-use-package '(nerd-icons :fork (:repo "fuzy112/nerd-icons.el")))
(straight-use-package 'nerd-icons-completion)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'nerd-icons-ibuffer)
(straight-use-package '(nerd-icons-multimodal :host github :repo "abougouffa/nerd-icons-multimodal"))
(straight-use-package '(nerd-icons-grep :host github :repo "hron/nerd-icons-grep"))

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
(straight-use-package 'consult-git-log-grep)

;;; eldoc
(straight-use-package 'eldoc-box)
(straight-use-package '(eldoc-diffstat :host github :repo "kljohann/eldoc-diffstat"))

;;; Input methods
(straight-use-package 'pyim)
(straight-use-package 'pyim-basedict)
(unless emacs-is-installed-by-nix
  (straight-use-package '(rime :fork (:host github :repo "fuzy112/emacs-rime")
			       :pre-build ("make" "lib")
			       :files (:defaults "lib*.so"))))

;;; AI/LLM
(straight-use-package '(gptel :fork (:host github :repo "fuzy112/gptel")
			      :build (:not compile)))
(straight-use-package '(gptel-agent :host github :repo "karthink/gptel-agent"
				    :files (:defaults "agents")))
(straight-use-package 'gptel-aibo)
(straight-use-package '(claude-code-ide :host github :repo "manzaltu/claude-code-ide.el"))
(straight-use-package '(whisper :host github :repo "natrys/whisper.el"))

;;; Modal editing
(straight-use-package 'meow)
(straight-use-package 'meow-tree-sitter)
(straight-use-package '(repeat-fu :host codeberg :repo "ideasman42/emacs-repeat-fu"))

;;; Help and documentation

(straight-use-package 'devdocs)

;;; Undo
(straight-use-package 'vundo)
(straight-use-package 'undo-fu-session)

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

;;; Encryption

(straight-use-package 'agenix)

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
(unless emacs-is-installed-by-nix
  (straight-use-package '(pdf-tools
			  :pre-build ("sh" "-c" "cd server && ./autobuild")
			  :files (:defaults "server/epdfinfo"))))
(straight-use-package '(pcap-mode :fork (:host github :repo "fuzy112/pcap-mode")))
(straight-use-package 'nix-mode)

;;; Applications
(unless emacs-is-installed-by-nix
  (straight-use-package '(telega :pre-build ("make" "telega-server")
				 :files (:defaults "etc" "contrib" "server/telega-server"))))
(straight-use-package 'ement)
(straight-use-package 'show-font)
(straight-use-package '(elfeed :fork (:host github :repo "fuzy112/elfeed")))
(straight-use-package '(eat :files (:defaults "integration" "term" "terminfo")
			    :fork (:host codeberg :repo "fuzy/emacs-eat")))
(straight-use-package '(video-trimmer :host github :repo "xenodium/video-trimmer"
				      :fork (:host github :repo "fuzy112/video-trimmer")))
(straight-use-package '(whisper :host github :repo "natrys/whisper.el"))
(unless emacs-is-installed-by-nix
  (straight-use-package
   `(reader :type git :host codeberg :repo "divyaranjan/emacs-reader"
	    :files ("*.el" "render-core.so")
	    :pre-build ("cc" "-fPIC" "-shared" "-o" "render-core.so"
			"-DLINUX" "render/elisp-helpers.c"
			"render/mupdf-helpers.c" "render/render-core.c"
			"render/render-theme.c" "render/render-threads.c"
			"-lmupdf"))))

;;; Email
(straight-use-package 'autocrypt)
