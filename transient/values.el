((ctags-menu
  ("--exclude=" ".git")
  "--recurse"
  ("--languages=" "C" "C++" "CMake")
  "--extras=*" "--fields=*" "--kinds-all=*" "--roles-all=*")
 (gptel-menu "g*quick-answer*")
 (magit-am "--3way" "--gpg-sign=ABE50B31E2F0C94AC4585BC78D97BF3F6BFA0BDA")
 (magit-cherry-pick "--ff" "--gpg-sign=ABE50B31E2F0C94AC4585BC78D97BF3F6BFA0BDA")
 (magit-commit "--gpg-sign=ABE50B31E2F0C94AC4585BC78D97BF3F6BFA0BDA")
 (magit-log:magit-log-mode "-n256" "--graph" "--decorate" "--show-signature")
 (magit-merge "--gpg-sign=ABE50B31E2F0C94AC4585BC78D97BF3F6BFA0BDA")
 (magit-rebase "--autostash" "--gpg-sign=ABE50B31E2F0C94AC4585BC78D97BF3F6BFA0BDA"))
