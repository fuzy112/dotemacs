;;; autocrypt.el --- Autocrypt implementation -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; Version: 0.4.2
;; Keywords: comm
;; Package-Requires: ((emacs "24.3"))
;; URL: https://git.sr.ht/~pkal/autocrypt

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of Autocrypt (https://autocrypt.org/) for various
;; Emacs MUAs.

;; Run M-x `autocrypt-create-account' to initialise an autocrypt key,
;; and add `autocrypt-mode' to your MUA's hooks (`gnus-mode-hook',
;; `message-mode-hook', ...) to activate it's usage.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'rx))
(require 'epg)
(require 'ietf-drums)


;;;; Customizables

(defgroup autocrypt nil
  "Autocrypt protocol implementation for Emacs MUAs."
  :tag "Autocrypt"
  :group 'mail
  :link '(url-link "https://autocrypt.org/")
  :prefix "autocrypt-")

(defcustom autocrypt-accounts nil
  "Alist of supported Autocrypt accounts.

All elements have the form (MAIL FINGERPRINT PREFERENCE), where
FINGERPRINT is the fingerprint of the PGP key that should be used
by email address MAIL.  PREFERENCE must be one of `mutual' or
`no-preference', `none' (if no preference should be inserted into
headers), or nil if this account should be temporarily disabled.

This variable doesn't have to be manually specified, as
activating the command `autocrypt-mode' should automatically
configure it, or by calling `autocrypt-create-account'."
  :type '(alist :key-type (string :tag "Address")
                :value-type
                (group (string :tag "Fingerprint")
                       (choice :tag "Encryption Preference"
                               (const :tag "None" none)
                               (const :tag "No Preference" no-preference)
                               (const :tag "Mutual" mutual)
                               (const :tag "Disable this Account" nil)))))

(defcustom autocrypt-do-gossip t
  "Enable Autocrypt gossiping.

This will inject \"Autocrypt-Gossip\" headers when required, and
process \"Autocrypt-Gossip\" headers when received."
  :type '(choice (const :tag "Enable Gossip" t)
                 (const :tag "Only receive" only-receive)
                 (const :tag "Only send" only-send)
                 (const :tag "Disable Gossip" nil)))

(defcustom autocrypt-save-file
  (locate-user-emacs-file "autocrypt-data.el")
  "File where Autocrypt peer data should be saved."
  :type '(file :must-match t))


;;;; Data Structures

;; https://autocrypt.org/level1.html#communication-peers
(cl-defstruct (autocrypt-peer (:constructor autocrypt-make-peer))
  last-seen
  timestamp
  pubkey
  preference
  gossip-timestamp
  gossip-key
  deactivated)


;;; Internal State

(defvar autocrypt-peers nil
  "List of known autocrypt peers.

Every member of this list has to be an instance of the
`autocrypt-peer' structure.")

(defconst autocrypt-save-variables
  '(autocrypt-peers)
  "List of variables to save to `autocrypt-save-data'.")

(defvar autocrypt-loaded-version)       ;used by `autocrypt-load-data'


;;;; Mua Translation Layer

(defvar autocrypt-backends
  (list (lambda () (and (derived-mode-p 'mu4e-main-mode 'mu4e-view-mode) 'mu4e))
        (lambda () (and (derived-mode-p 'gnus-mode) 'gnus))
        (lambda () (and (derived-mode-p 'rmail-mode) 'rmail))
        (lambda () (and (derived-mode-p 'message-mode) 'message))
        (lambda ()
          ;; Guess backend from major mode
          (when (string-match "-mode\\'" (symbol-name major-mode))
            (intern (substring (symbol-name major-mode) 0 (match-beginning 0))))))
  "Hook for finding a backend.
Each function is called without any further arguments, and should
return a symbol designating the backend to use.")

(defvar-local autocrypt-backend-function nil
  "Override the function called by `autocrypt-find-function'.
This function must accept one argument, a symbol designating the
command (`install', `get-header', ...) and returns a function
with the right signature.")

(defun autocrypt-find-function (command silent)
  "Return a function for handling COMMAND.
If SILENT is non-nil, return nil when no implementation could be
found."
  (if autocrypt-backend-function
      (funcall autocrypt-backend-function command)
    (let ((backend (run-hook-with-args-until-success 'autocrypt-backends)))
      (unless backend
        (error "No backend found"))
      (catch 'ok
        (dolist (fmt '("autocrypt-%S--%S" "%S-autocrypt-%S" "%S--autocrypt-%S"))
          (let ((fn (intern-soft (format fmt backend command))))
            (when (fboundp fn) (throw 'ok fn))))
        (unless silent
          (error "Missing %S implementation for %S" command backend))))))

(defun autocrypt-make-function (command signature &optional default)
  "Return a function to handle COMMAND.
The advertised calling convention is set to SIGNATURE.  If
DEFAULT is non-nil, return DEFAULT when no implementation could
be found."
  (let ((fn (lambda (&rest args)
              (let* ((silent (not (null default)))
                     (fn (autocrypt-find-function command silent)))
                (if fn (apply fn args) default)))))
    (set-advertised-calling-convention fn signature nil)
    fn))

(defalias 'autocrypt-install
  (autocrypt-make-function 'install '())
  "Install necessary autocrypt functions into the MUA.
A mail reader (Gnus, Rmail, ...) should only needs to implement
`get-header' and optionally `get-part'.  A
composer (message-mode) should ideally have an implementation the
generic functions `get-header', `add-header', `remove-header',
`sign-encrypt', `secure-attach' and `encrypted-p'.

This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-uninstall
  (autocrypt-make-function 'uninstall '())
  "Remove all modifications by autocrypt.
This should reverse the effect of `autocrypt-install'.

This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-get-header
  (autocrypt-make-function 'get-header '(header))
  "Return the value of HEADER.
This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-add-header
  (autocrypt-make-function 'add-header '(header value))
  "Insert HEADER with VALUE into message.
This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-remove-header
  (autocrypt-make-function 'remove-header '(header))
  "Remove HEADER from message.
This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-sign-encrypt
  (autocrypt-make-function 'sign-encrypt '())
  "Make the message to be signed and encrypted.
This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-secure-attach
  (autocrypt-make-function 'secure-attach '(payload))
  "Add PAYLOAD as an encrypted attachment.
This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-encrypted-p
  (autocrypt-make-function 'encrypted-p '())
  "Check the the current message is encrypted.
This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")

(defalias 'autocrypt-get-part
  (autocrypt-make-function 'get-part '(index) 'n/a)
  "Return the INDEX'th part of the current message.
This is a generic function and the behaviour varies depending on
the buffer it is being invoked in.  See `autocrypt-backends' and
`autocrypt-make-function' for more details.")


;;;; Internal Functions

;; https://autocrypt.org/level1.html#e-mail-address-canonicalization
(defun autocrypt-canonicalise (addr)
  "Return a canonical form of email address ADDR."
  ;; "[...] Other canonicalization efforts are considered for later
  ;; specification versions."
  (save-match-data
    (let ((parts (mail-extract-address-components addr)))
      (downcase (cadr parts)))))

(defun autocrypt-load-data ()
  "Load peer data if exists from `autocrypt-save-file'."
  (when (file-exists-p autocrypt-save-file)
    (load autocrypt-save-file t t t)
    (when (boundp 'autocrypt-loaded-version)
      ;; handle older versions if necessary
      t)))

(defun autocrypt-save-data ()
  "Write peer data save-file to `autocrypt-save-file'."
  (with-temp-buffer
    (insert ";; generated by autocrypt.el	-*- mode: emacs-lisp -*-\n"
            ";; do not modify by hand\n")
    (let ((standard-output (current-buffer)))
      (when (fboundp 'package-get-version)
        (print `(setq autocrypt-loaded-version ,(package-get-version))))
      (dolist (var autocrypt-save-variables)
        (print `(unless ,var
                  (setq ,var ',(symbol-value var))))))
    (write-region nil nil autocrypt-save-file nil 'silent)))

;; https://autocrypt.org/level1.html#recommendations-for-single-recipient-messages
(defun autocrypt-single-recommentation (sender recipient)
  "Calculate autocrypt recommendation for a single RECIPIENT.

SENDER is a string containing the value of the From field."
  (let ((self (cdr (assoc (autocrypt-canonicalise sender) autocrypt-accounts)))
        (peer (cdr (assoc (autocrypt-canonicalise recipient) autocrypt-peers))))
    ;; TODO: check if keys are revoked, expired or unusable
    (cond
     ((not (and peer (autocrypt-peer-pubkey peer)))
      'disabled)
     ((and (eq (autocrypt-peer-preference peer) 'mutual)
           (eq (cadr self) 'mutual))
      'encrypt)
     ((or (eq (autocrypt-peer-preference peer) 'mutual)
          (> (- (time-to-days (current-time))
                (time-to-days (autocrypt-peer-last-seen peer)))
             35))
      'discourage)
     (t 'available))))

;; https://autocrypt.org/level1.html#provide-a-recommendation-for-message-encryption
(defun autocrypt-recommendation (sender recipients)
  "Calculate a autocrypt recommendation for RECIPIENTS.

SENDER is a string containing the value of the From field."
  (let ((res (mapcar (apply-partially #'autocrypt-single-recommentation sender)
                     recipients)))
    (cond
     ((or (null res) (memq 'disabled res)) 'disabled)
     ((memq 'discourage res) 'discourage)
     ((null (delq 'encrypt res)) 'encrypt)
     (t 'available))))

;; https://autocrypt.org/level1.html#the-autocrypt-header
(defun autocrypt-parse-header (string)
  "Destruct Autocrypt header from STRING.

Returns list with address, preference and key data if
well-formed, otherwise returns just nil."
  (with-temp-buffer
    (save-excursion
      (insert string))
    (let (addr pref keydata)
      (save-match-data
        (while (looking-at (rx (group (+ (any alnum ?_ ?-)))
                               (* space) "=" (* space)
                               (group (* (not (any ";"))))
                               (? ";" (* space))))
          (cond
           ((string= (match-string 1) "addr")
            (setq addr (autocrypt-canonicalise (match-string 2))))
           ((string= (match-string 1) "prefer-encrypt")
            (setq pref (cond
                        ((string= (match-string 2) "mutual")
                         'mutual)
                        ((string= (match-string 2) "nopreference")
                         'no-preference))))
           ((string= (match-string 1) "keydata")
            (setq keydata (base64-decode-string (match-string 2))))
           ((string-prefix-p "_" (match-string 1)) ; ignore
            nil)
           (t (error "Unsupported Autocrypt key")))
          (goto-char (match-end 0))))
      (and addr keydata (list addr pref keydata)))))

(defun autocrypt-list-recipients ()
  "Return a list of all recipients to this message."
  (let (recipients)
    (dolist (header '("To" "Cc" "Reply-To"))
      (let* ((f (autocrypt-get-header header))
             (r (and f (mail-extract-address-components f t))))
        (setq recipients (nconc (mapcar #'cadr r) recipients))))
    (delete-dups recipients)))

;; https://autocrypt.org/level1.html#updating-autocrypt-peer-state-from-key-gossip
(defun autocrypt-process-gossip (date)
  "Update internal autocrypt gossip state.

Argument DATE contains the time value of the \"From\" tag."
  (let ((recip (autocrypt-list-recipients))
        (root (autocrypt-get-part 0))
        (re (rx bol "Autocrypt-Gossip:" (* space)
                (group (+ (or nonl (: "\n "))))
                eol))
        gossip)
    (unless (eq root 'n/a)
      (catch 'unsupported
        (with-temp-buffer
          ;; The MUA interface should have the flexibility to return
          ;; different kinds of part containers. Currently
          ;; buffers, strings and a cons-cell of the form '(file . PATH)
          ;; are supported, where PATH is a string describing where to
          ;; find the part.
          (cond
           ((bufferp root)
            (insert (with-current-buffer root (buffer-string))))
           ((stringp root)
            (insert root))
           ((eq (car root) 'file)
            (insert-file-contents (cdr root)))
           (t
            ;; If unsupported, then the gossip processing should fail
            ;; SILENTLY. Autocrypt should not annoy the user.
            (throw 'unsupported nil)))
          (ietf-drums-narrow-to-header)
          (goto-char (point-min))
          (save-match-data
            (while (search-forward-regexp re nil t)
              (push (autocrypt-parse-header (match-string 1))
                    gossip))))
        (dolist (datum (delq nil gossip))
          (let* ((addr (car datum))
                 (peer (cdr (assoc addr recip))))
            (if (and peer (time-less-p (autocrypt-peer-gossip-timestamp peer)
                                       date))
                (setf (autocrypt-peer-gossip-timestamp date)
                      (autocrypt-peer-gossip-key (caddr datum)))
              (push (cons addr (autocrypt-make-peer
                                :gossip-timestamp date
                                :gossip-key (caddr datum)))
                    autocrypt-peers))))
        (autocrypt-save-data)))))

;; https://autocrypt.org/level1.html#updating-autocrypt-peer-state
(defun autocrypt-process-header ()
  "Update internal autocrypt state."
  (let* ((from-raw (autocrypt-get-header "From"))
         (from (and from-raw (autocrypt-canonicalise from-raw)))
         (date (autocrypt-get-header "Date"))
         (header (autocrypt-get-header "Autocrypt"))
         parse addr preference keydata peer)
    (when (and from date)
      (setq date (ietf-drums-parse-date date))
      (when header
        (when (setq parse (autocrypt-parse-header header))
          (setq addr (autocrypt-canonicalise (car parse))
                preference (cadr parse)
                keydata (caddr parse)
                peer (or (cdr (assoc addr autocrypt-peers))
                         (autocrypt-make-peer
                          :last-seen date
                          :timestamp date
                          :pubkey keydata
                          :preference preference)))))
      (when (memq autocrypt-do-gossip '(t only-receive))
        (autocrypt-process-gossip date))
      (when (string= from addr)
        (unless (time-less-p date (autocrypt-peer-timestamp peer))
          (when (time-less-p (autocrypt-peer-last-seen peer) date)
            (setf (autocrypt-peer-last-seen peer) date))
          (if keydata                         ; has "Autocrypt" header
              (setf (autocrypt-peer-preference peer) (or preference 'none)
                    (autocrypt-peer-deactivated peer) nil
                    (autocrypt-peer-timestamp peer) date
                    (autocrypt-peer-pubkey peer) keydata)
            (setf (autocrypt-peer-deactivated peer) t))
          (unless (assoc addr autocrypt-peers)
            (push (cons addr peer) autocrypt-peers))
          (autocrypt-save-data))))))

(defun autocrypt-insert-keydata (data)
  "Insert raw keydata DATA as base64 at point."
  (let ((start (point)))
    (insert data)
    (base64-encode-region start (point))
    (save-restriction
      (narrow-to-region start (point))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (insert " ")
          (forward-line))))))

(defun autocrypt-get-keydata (acc)
  "Generate key for ACC to insert into header."
  (let* ((ctx (epg-make-context))
         (keys (epg-list-keys ctx (cadr acc) 'secret)))
    (epg-export-keys-to-string ctx keys)))

(defun autocrypt-generate-header (addr &optional gossip-p)
  "Generate header value for address ADDR.

If ADDR is a local account, it's key will be used.  Otherwise it
attempts to look up ADDR in the peer data.  If nothing was found
OR the header is too large, return nil.  If GOSSIP-P is non-nil,
the function will not add an encryption
preference (\"prefer-encrypt\")."
  (let (acc peer pref keydata)
    (cond
     ((setq acc (assoc addr autocrypt-accounts))
      (setq keydata (autocrypt-get-keydata acc)
            pref (caddr acc)))
     ((setq peer (cdr (assoc addr autocrypt-peers)))
      (setq keydata (autocrypt-peer-pubkey peer)
            pref (autocrypt-peer-preference peer))))
    (when (and (not (and peer (autocrypt-peer-deactivated peer)))
               pref keydata)
      (with-temp-buffer
        (insert "addr=" addr "; ")
        (cond
         (gossip-p
          ;; Key Gossip Injection [...] SHOULD NOT include a
          ;; prefer-encrypt attribute.
          nil)
         ((eq pref 'no-preference)
          (insert "prefer-encrypt=nopreference; "))
         ((eq pref 'mutual)
          (insert "prefer-encrypt=mutual; ")))
        (insert "keydata=\n")
        (autocrypt-insert-keydata keydata)
        (and (< (buffer-size) (* 10 1024))
             (buffer-string))))))

(defun autocrypt-gossip-p (recipients)
  "Find out if the current message should have gossip headers.
Argument RECIPIENTS is a list of addresses this message is
addressed to."
  (and (memq autocrypt-do-gossip '(t only-send))
       (autocrypt-encrypted-p)
       (< 1 (length recipients))
       (cl-every
        (lambda (rec)
          (let ((peer (cdr (assoc rec autocrypt-peers))))
            (and peer (not (autocrypt-peer-deactivated peer)))))
        recipients)))

(defun autocrypt-compose-setup ()
  "Check if Autocrypt is possible, and add pseudo headers."
  (interactive)
  (let ((recs (autocrypt-list-recipients))
        (from (autocrypt-canonicalise (autocrypt-get-header "From"))))
    ;; encrypt message if applicable
    (save-excursion
      (cl-case (autocrypt-recommendation from recs)
        (encrypt
         (autocrypt-sign-encrypt))
        (available
         (autocrypt-add-header "Do-Autocrypt" "no"))
        (discourage
         (autocrypt-add-header "Do-Discouraged-Autocrypt" "no"))))))

(defun autocrypt-compose-pre-send ()
  "Insert Autocrypt headers before sending a message.

Will handle and remove \"Do-(Discourage-)Autocrypt\" if found."
  (let* ((recs (autocrypt-list-recipients))
         (from (autocrypt-canonicalise (autocrypt-get-header "From"))))
    ;; encrypt message if applicable
    (when (eq (autocrypt-recommendation from recs) 'encrypt)
      (autocrypt-sign-encrypt))
    ;; check for manual autocrypt confirmations
    (let ((do-autocrypt (autocrypt-get-header "Do-Autocrypt"))
          (ddo-autocrypt (autocrypt-get-header "Do-Discouraged-Autocrypt"))
          (query "Are you sure you want to use Autocrypt, even though it is discouraged?"))
      (when (and (not (autocrypt-encrypted-p))
                 (or (and do-autocrypt
                          (string= (downcase do-autocrypt) "yes"))
                     (and ddo-autocrypt
                          (string= (downcase ddo-autocrypt) "yes")
                          (yes-or-no-p query))))
        (autocrypt-sign-encrypt)))
    (autocrypt-remove-header "Do-Autocrypt")
    (autocrypt-remove-header "Do-Discouraged-Autocrypt")
    ;; insert gossip data
    (when (autocrypt-gossip-p recs)
      (let ((payload (generate-new-buffer " *autocrypt gossip*")))
        (with-current-buffer payload
          (dolist (addr (autocrypt-list-recipients))
            (let ((header (autocrypt-generate-header addr t)))
              (insert "Autocrypt-Gossip: " header "\n"))))
        (autocrypt-secure-attach payload)))
    ;; insert autocrypt header
    (let ((header (and from (autocrypt-generate-header from))))
      (when header
        (autocrypt-add-header "Autocrypt" header)))))

;;;###autoload
(defun autocrypt-create-account ()
  "Create a GPG key for Autocrypt."
  (interactive)
  (let ((ctx (epg-make-context))
        (name (read-string "Name: " user-full-name))
        (email (read-string "Email: " user-mail-address))
        (expire (read-string "Expire Date (date or <n>{d,w,m,y}): "))
        (pass (let ((prompt "Passphrase (must be non-empty): ") pass)
                (while (eq (setq pass (read-passwd prompt t t)) t))
                pass)))
    (epg-generate-key-from-string
     ctx
     (with-temp-buffer
       ;; https://www.gnupg.org/documentation/manuals/gnupg/Unattended-GPG-key-generation.html
       ;; https://lists.gnupg.org/pipermail/gnupg-users/2017-December/059622.html
       (insert "Key-Type: eddsa\n")
       (insert "Key-Curve: Ed25519\n")
       (insert "Key-Usage: sign\n")
       (insert "Subkey-Type: ecdh\n")
       (insert "Subkey-Curve: Curve25519\n")
       (insert "Subkey-Usage: encrypt\n")
       (insert "Name-Real: " name "\n")
       (insert "Name-Email: " email "\n")
       (insert "Name-Comment: generated by autocrypt.el\n")
       (insert "Passphrase: " pass "\n")
       (insert "Expire-Date: " (if (string= "" expire) "0" expire) "\n")
       (insert "%commit")
       (buffer-string)))
    (let ((res (epg-context-result-for ctx 'generate-key)))
      (unless res
        (error "Could not determine fingerprint"))
      (customize-save-variable
       'autocrypt-accounts
       (cons (list email (cdr (assq 'fingerprint (car res))) 'none)
             autocrypt-accounts)
       "Set by autocrypt.el"))
    (message "Successfully generated key for %s, and added to key chain."
             email)))


;;;; Minor Modes

;;;###autoload
(define-minor-mode autocrypt-mode
  "Enable Autocrypt support in current buffer."
  :group 'autocrypt
  (if autocrypt-mode
      (progn
        (autocrypt-load-data)
        (autocrypt-install))
    (autocrypt-uninstall)))

(provide 'autocrypt)

;;; autocrypt.el ends here
