org-mode

(header "# -*- mode: org; coding: utf-8; -*-" n n
	"#+title: " (p "title") n
	"#+author: " user-full-name "  <" user-mail-address ">" n
	"#+date: " (current-time-string) n
	"#+startup: fold" n)

(src "#+begin_src " (p "lang") n
     p n
     "#+end_src" n)

(kbd "{{{kbd(" p ")}}}")
