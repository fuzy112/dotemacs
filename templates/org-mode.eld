;; -*- coding: utf-8-unix; -*-
org-mode

(header "# -*- mode: org; coding: utf-8-unix; -*-" n n
	"#+TITLE:    " (p "title") n
	"#+AUTHOR:   " user-full-name "  <" user-mail-address ">" n
	"#+DATE:     " (current-time-string) n
	":" n
	"#+STARTUP:  fold" n
	"#+STARTUP:  indent" n)

(src "#+begin_src " (p "lang") n
     p n
     "#+end_src" n)

(quote "#+begin_quote " n
     p n
     "#+end_quote" n)

(kbd "{{{kbd(" p ")}}}")
