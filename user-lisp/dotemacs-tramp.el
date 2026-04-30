;;; dotemacs-tramp.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;;;; tramp

(after-load! tramp
  (setopt tramp-verbose 2))

(defvar tramp-persistency-file-name)
(define-advice tramp-dump-connection-properties (:after () chmod)
  "Change permission of `tramp-persistency-file-name' to 0600."
  (when (file-readable-p tramp-persistency-file-name)
    (chmod tramp-persistency-file-name #o600)))

;;; We need to ensure Unix domain sockets have paths shorter than 108 characters
;;; (this is a system limit). If tramp-compat-temporary-file-directory is too long,
;;; we'll use a shorter alternative location.
(defvar tramp-compat-temporary-file-directory)
(after-load! tramp-compat
  ;; Check if the current directory path is longer than 50 characters.
  ;; This provides a safety margin since socket names will be appended to this path.
  (when (length> tramp-compat-temporary-file-directory 50)
    ;; Set a shorter alternative directory in XDG_RUNTIME_DIR
    (setq tramp-compat-temporary-file-directory
          (substitute-in-file-name "$XDG_RUNTIME_DIR/emacs"))
    ;; Create the directory if it doesn't exist yet
    ;; The 't' parameter creates parent directories as needed
    (unless (file-directory-p tramp-compat-temporary-file-directory)
      (mkdir tramp-compat-temporary-file-directory t))
    (chmod tramp-compat-temporary-file-directory #o700)))

(after-load! tramp
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profile-variables
   'remote-explicit-shell-file-name
   '((explicit-shell-file-name . "/bin/sh")))

  (dolist (protocol '("ssh" "sshx" "scp" "scpx"))
    (connection-local-set-profiles
     `(:application tramp :protocol ,protocol)
     'remote-explicit-shell-file-name
     'remote-direct-async-process)))

(provide 'dotemacs-tramp)
;;; dotemacs-tramp.el ends here
