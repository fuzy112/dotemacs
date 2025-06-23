((ctags-menu
  ("--exclude=" ".git")
  "--recurse"
  ("--languages=" "C" "C++" "CMake")
  "--extras=*" "--fields=*" "--kinds-all=*" "--roles-all=*")
 (gptel-menu "g*quick-answer*")
 (magit-fetch "--prune" "--tags")
 (magit-log:magit-log-mode "-n256" "--graph" "--decorate" "--show-signature")
 (magit-pull "--autostash"))
