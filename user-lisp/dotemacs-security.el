;;; dotemacs-security.el  -*- lexical-binding: t; -*-

(eval-when-compile (require 'dotemacs-core))

;;;; auth-sources
(after-load! auth-sources
  (when (custom--standard-value-p 'auth-sources auth-sources)
    (setopt auth-sources '("~/.authinfo.gpg")))
  (setopt auth-source-save-behavior t
          auth-source-gpg-encrypt-to (list  "0xBBE2757FC7BFC23B"))
  (auth-source-forget-all-cached))

;;;; epg

(after-load! epa
  (setopt epa-keys-select-method 'minibuffer))

(defun send-password-to-process (process)
  "Read a password and send it to the process in BUFFER."
  (interactive
   (list
    (if-let* ((proc (get-buffer-process (current-buffer)))
              ((not current-prefix-arg)))
        proc
      (read-process-name "Process"))))
  (process-send-string process
                       (concat
                        (read-passwd "Password: ")
                        "\n")))

(provide 'dotemacs-security)
;;; dotemacs-security.el ends here
