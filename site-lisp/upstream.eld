;; site-lisp upstreams

(:package debian-el
	  :git "https://salsa.debian.org/emacsen-team/debian-el"
	  :files ("apt-sources.el"
		  "apt-utils.el"
		  "deb-view.el"
		  "debian-bug.el"
		  "debian-el.el"
		  "gnus-BTS.el"
		  "preseed.el"))

(:package dpkg-dev-el
	  :git "https://salsa.debian.org/emacsen-team/dpkg-dev-el"
	  :files ("debian-bts-control.el"
		  "debian-changelog-mode.el"
		  "debian-control-mode.el"
		  "debian-copyright.el"
		  "debian-autopkgtest-control-mode.el"
		  "dpkg-dev-el.el"
		  "readme-debian.el"))

(:package copilot
	  :git "https://github.com/copilot-emacs/copilot.el"
	  :files ("copilot.el"
		  "copilot-balancer.el"))

(:package copilot-chat
	  :git "https://github.com/chep/copilot-chat.el"
	  :files ("copilot-chat.el"
		  "copilot-chat-copilot.el"))

(:package outline-minor-faces
	  :git "https://github.com/tarsius/outline-minor-faces"
	  :files ("outline-minor-faces.el"))

(:package s
	  :git "https://github.com/magnars/s.el"
	  :files ("s.el"))

(:package f
	  :git "http://github.com/rejeep/f.el"
	  :files ("f.el"))

(:package consult-dir
	  :git "https://github.com/karthink/consult-dir"
	  :files ("consult-dir.el"))

(:package hl-todo
	  :git "https://github.com/tarsius/hl-todo"
	  :files ("hl-todo.el"))

(:package apheleia
	  :git "https://github.com/radian-software/apheleia"
	  :files ("apheleia-dp.el"
		  "apheleia-formatter-context.el"
		  "apheleia-formatters.el"
		  "apheleia-log.el"
		  "apheleia-rcs.el"
		  "apheleia-utils.el"
		  "apheleia.el")
	  :tag "v4.2")

(:package nerd-icons.el
	  :git "https://github.com/rainstormstudio/nerd-icons.el"
	  :files ("nerd-icons.el"
		  "nerd-icons-faces.el"
		  "nerd-icons-data.el"
		  "nerd-icons-data-codicon.el"
		  "nerd-icons-data-devicon.el"
		  "nerd-icons-data-faicon.el"
		  "nerd-icons-data-flicon.el"
		  "nerd-icons-data-ipsicon.el"
		  "nerd-icons-data-mdicon.el"
		  "nerd-icons-data-octicon.el"
		  "nerd-icons-data-pomicon.el"
		  "nerd-icons-data-powerline.el"
		  "nerd-icons-data-sucicon.el"
		  "nerd-icons-data-wicon.el"))

(:package nerd-icons-completion
	  :git "https://github.com/rainstormstudio/nerd-icons-completion"
	  :files ("nerd-icons-completion.el"))

(:package nerd-icons-corfu
	  :git "https://github.com/LuigiPiucco/nerd-icons-corfu"
	  :files ("nerd-icons-corfu.el"))

(:package nerd-icons-dired
	  :git "https://github.com/rainstormstudio/nerd-icons-dired"
	  :files ("nerd-icons-dired.el"))

(:package nerd-icons-ibuffer
	  :git "https://github.com/seagle0128/nerd-icons-ibuffer"
	  :files ("nerd-icons-ibuffer.el"))

(:package term-keys
	  :git "https://github.com/CyberShadow/term-keys"
	  :files ("term-keys.el"
		  "term-keys-glfw-mods.el"
		  "term-keys-kitty.el"
		  "term-keys-konsole.el"
		  "term-keys-linux.el"
		  "term-keys-st.el"
		  "term-keys-terminal-app.el"
		  "term-keys-urxvt.el"
		  "term-keys-wezterm.el"
		  "term-keys-windows-terminal.el"
		  "term-keys-x11.el"
		  "term-keys-xterm.el"))

(:package goggles
	  :git "https://github.com/minad/goggles"
	  :files ("goggles.el"))

(:package xterm-color
	  :git "https://github.com/atomontage/xterm-color"
	  :files ("xterm-color.el"))
