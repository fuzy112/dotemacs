;; telega-config.el -- Telega configuration -*- lexical-binding: t; -*-

;; Copyright © 2024-2026  Zhengyi Fu <i@fuzy.me>

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'dotemacs-core)
(require 'telega-customize)

(unless emacs-is-installed-by-nix
  (setq telega-server-command (expand-file-name "telega-server"
                                                (file-name-directory (locate-library "telega")))))
(setq telega-translate-to-language-by-default "zh")
(setq telega-emoji-use-images nil)

;; (defun telega-config-workaround-gaps-on-graphic-display ()
;;   "Enable avatar gap workaround when running on a graphic display."
;;   (when (display-graphic-p)
;;     (setq-local telega-avatar-workaround-gaps-for '(return t))))
;; (add-hook 'telega-chat-mode-hook #'telega-config-workaround-gaps-on-graphic-display)

(declare-function telega-chat-auto-fill-mode "telega-chat.el" (&optional arg1))
(add-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)

(after-load! telega
  (telega-notifications-mode))

(after-load! consult
  (add-to-list 'consult-buffer-filter (concat "\\`" (regexp-quote (telega-symbol 'telegram))))
  (add-to-list 'consult-buffer-filter "\\`\\*Telega"))

(defvar telega-cache-dir)
(define-advice telega (:around (&rest args) default-directory)
  (let ((default-directory "~/"))
    (apply args)))

(add-hook 'telega-root-mode-hook #'hl-line-mode)

;; Telega company

;; Transform telega-company backends into Capf functions for seamless Corfu integration.
(defvar telega-capfs nil)
(let (capfs)
  (dolist (backend telega-company-backends)
    (let* ((backend-str (symbol-name backend))
           (suffix (string-remove-prefix "telega-company-" backend-str))
           (capf-name (intern (format "telega-capf-%s" suffix))))
      (defalias capf-name
        (cape-capf-properties
         (cape-company-to-capf backend)
         :company-kind (lambda (_) backend)))
      (push capf-name capfs)))
  (setq telega-capfs (nreverse capfs)))

;;; Configure nerd-icons-corfu for telega completions
(after-load! nerd-icons-corfu
  (alist-setq! nerd-icons-corfu-mapping
    telega-company-username '(:style "fa" :icon "user" :face default)
    telega-company-emoji '(:style "md" :icon "sticker_emoji" :face default)
    telega-company-telegram-emoji '(:style "md" :icon "sticker_emoji" :face default)
    telega-company-botcmd '(:style "cod" :icon "symbol_class" :face font-lock-builtin-face)
    telega-company-hashtag '(:style "fa" :icon "hashtag" :face default)
    telega-company-markdown-precode '(:style "cod" :icon "code_tags" :face font-lock-comment-face)
    telega-company-quick-reply '(:style "fa" :icon "comment" :face default)))

(defmacro telega-capf-trigger (capf trigger)
  `(define-advice ,capf (:around (fn &rest _) trigger)
     (cape-wrap-trigger fn ,trigger)))

(telega-capf-trigger telega-capf-emoji ?:)
(telega-capf-trigger telega-capf-telegram-emoji ?:)
(telega-capf-trigger telega-capf-botcmd ?/)
(telega-capf-trigger telega-capf-hashtag ?#)

(define-advice telega-capf-username (:around (capf) prefix)
  (when-let* ((pos (or (save-excursion (re-search-backward "@@" (pos-bol) 'noerror))
                       (save-excursion (re-search-backward "@" (pos-bol) 'noerror))))
              (len (length (match-string-no-properties 0)))
              ((save-excursion (not (re-search-backward "\\s-" pos 'noerror)))))
    (pcase
        (funcall capf)
      (`(,beg ,end ,table . ,plist)
       (when (<= pos beg (+ len pos))
         `( ,(+ len pos) ,end ,table
            :company-prefix-length t
            :exit-function
            ,(let ((pos (copy-marker pos))
                   (end (copy-marker (+ len pos))))
               (lambda (str status)
                 (delete-region pos end)
                 (when-let* ((exit (plist-get plist :exit-function)))
                   (funcall exit str status))))
            . ,plist))))))

(advice-add #'telega-capf-username :around #'cape-wrap-buster)
(advice-add #'telega-capf-emoji :around #'cape-wrap-buster)
(advice-add #'telega-capf-telegram-emoji :around #'cape-wrap-buster)

(defun telega-capf-setup ()
  "Setup Telega completion-at-point functions for current buffer."
  (interactive)
  (when (require 'company nil t)
    (setq-local completion-at-point-functions (append telega-capfs (list 'cape-file 'cape-tex)))
    (setq-local corfu-auto t
                corfu-quit-no-match nil
                corfu-auto-trigger "#:/@")))

(define-advice telega-chatbuf-complete (:override () corfu)
  (completion-at-point))

;; (add-hook 'telega-chat-mode-hook #'telega-capf-setup)
(defun telega-completion-setup-corfu ()
  (setq-local corfu-auto t
              corfu-auto-trigger "#:/@"
              completion-at-point-functions
              (list (cape-capf-super #'telega-completion-emoji
                                     :with #'telega-completion-telegram-emoji)
                    #'telega-completion-username
                    (cape-capf-super #'telega-completion-botcmd
                                     #'telega-completion-quick-reply)
                    #'telega-completion-hashtag
                    t)))
(add-hook 'telega-chat-mode-hook #'telega-completion-setup-corfu)

(keymap-global-set "M-g t" telega-prefix-map)


(provide 'telega-config)
;;; telega-config.el ends here
