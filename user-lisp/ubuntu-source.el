;; ubuntu-source.el --- Download Ubuntu source packages   -*- lexical-binding: t; -*-
;; Copyright © 2025  Zhengyi Fu <i@fuzy.me>

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

(require 'dom)
(require 'seq)
(require 'url)

(defconst ubuntu-source-known-ubuntu-distros
  '("focal"
    "focal-updates"
    "jammy"
    "jammy-updates"
    "mantic"
    "mantic-updates"
    "noble"
    "noble-updates"
    "oracular"
    "oracular-updates"
    "plucky"
    "questing"))

(autoload 'apt-utils-choose-package "apt-utils.el"
  "Choose a Debian package name." )


(defun ubuntu-source-find-sources-for-binary-package-async (pkg distro callback &rest cbargs)
  "Find source package for binary package PKG for DISTRO asynchronously.
Call CALLBACK with (SRC-PKG SRC-VERSION SRC-URLS) when done."
  (url-retrieve (format "https://packages.ubuntu.com/%s/%s" distro pkg)
                (lambda (status)
                  (run-at-time 0 nil #'kill-buffer (current-buffer))
                  (condition-case err1
                      (if (plist-get status :error)
                          (funcall callback distro nil)
                        (goto-char (point-min))
                        (search-forward "\n\n" nil t)
                        (let* ((dom (libxml-parse-html-region (point) (point-max)))
                               (anchors (dom-by-tag dom 'a))
                               (urls (mapcar (lambda (anchor) (dom-attr anchor 'href))
                                             anchors))
                               (src-urls (seq-filter (apply-partially
                                                      #'string-match-p
                                                      "\\`https?://archive\\.ubuntu\\.com/ubuntu/pool/")
                                                     urls))
                               (file-names (mapcar #'url-file-nondirectory src-urls))
                               (dsc-file-name (seq-find (apply-partially #'string-suffix-p ".dsc")
                                                        file-names))
                               src-pkg
                               src-version)
                          (progn
                            (unless dsc-file-name
                              (error "No .dsc file"))
                            (unless (string-match "\\`\\([^_]+\\)_\\(.+\\)\\.dsc\\'" dsc-file-name)
                              (error "Invalid dsc file name: %s" dsc-file-name))
                            (setq src-pkg (match-string 1 dsc-file-name)
                                  src-version (match-string 2 dsc-file-name))
                            (apply callback `(,src-pkg ,src-version ,src-urls) cbargs))))
                    (error (apply callback (list :error err1) cbargs))))))

;; (ubuntu-source-find-sources-for-binary-package-async
;;  "curl" "jammy" (lambda (result)
;;                   (message "Result: %S" result)))

(defvar-local ubuntu-source--header-line-function nil)

(defun ubuntu-source-find-sources-for-multiple-distros (pkg callback &rest cbargs)
  "CALLBACK is called with a list of (distro . package)."
  (let ((remaining-distros ubuntu-source-known-ubuntu-distros)
        (fetching-distros nil)
        (found-packages nil)
        (state-buffer (get-buffer-create " *ubuntu-source*")))
    (with-current-buffer state-buffer
      (erase-buffer)
      (setq-local ubuntu-source--header-line-function
                  (lambda ()
                    (format "Fetching %d (%d/%d)"
                            (length fetching-distros)
                            (length remaining-distros)
                            (length ubuntu-source-known-ubuntu-distros))))
      (setq-local header-line-format
                  (list '(:eval (funcall ubuntu-source--header-line-function)))))
    (display-buffer state-buffer)
    (letrec ((intermediate-callback
              (lambda (result distro)
                (with-current-buffer state-buffer
                  (setq fetching-distros (remove distro fetching-distros))
                  (cond ((eq (car result) :error)
                         (insert (format "\nFailed to fetch sources for `%s'" distro)))
                        (t
                         (push (cons distro result) found-packages)
                         (insert (format "\nFound package %s_%s for `%s'"
                                         (car result)
                                         (cadr result)
                                         distro))))
                  (if (null remaining-distros)
                      (when (null fetching-distros)
                        (bury-buffer state-buffer)
                        (if (null found-packages)
                            (apply callback result cbargs)
                          (apply callback found-packages cbargs)))
                    (let ((new-distro (pop remaining-distros)))
                      (push new-distro fetching-distros)
                      (ubuntu-source-find-sources-for-binary-package-async
                       pkg new-distro intermediate-callback new-distro)))))))
      (setq fetching-distros (seq-take remaining-distros 4)
            remaining-distros (seq-drop remaining-distros 4))
      (dolist (distro fetching-distros)
        (ubuntu-source-find-sources-for-binary-package-async
         pkg distro intermediate-callback distro)))))


(defun ubuntu-source-download (pkg)
  (interactive (list (apt-utils-choose-package)))
  (let ((frame (selected-frame))
        (window (selected-frame)))
    (ubuntu-source-find-sources-for-multiple-distros
     pkg
     (lambda (result)
       (ignore-errors
         (select-frame-set-input-focus frame)
         (select-window window))

       (message "Current thread: %S" (current-thread))
       (cond ((eq (car result) :error)
              (error "Failed to fetch packages: %S" (cadr result)))
             (t
              (let* ((candidates (mapcar (lambda (package)
                                           (let ((p (nth 1 package))
                                                 (v (nth 2 package))
                                                 (d (nth 0 package))
                                                 (u (nth 3 package)))
                                             (list (concat p "_" v)
                                                   d
                                                   u)))
                                         result))
                     (affixation-function (lambda (completions)
                                            (seq-map (lambda (cand)
                                                       (list (car cand)
                                                             ""
                                                             (concat (propertize " " 'display '(space :align-to 40))
                                                                     (cadr cand))))
                                                     (seq-filter (lambda (cand)
                                                                   (member (car cand) completions))
                                                                 candidates))))
                     (metadata `(metadata (category . ubuntu-source-package)
                                          (affixation-function . ,affixation-function)))
                     (table (lambda (str pred action)
                              (if (eq action 'metadata)
                                  metadata
                                (complete-with-action action candidates str pred))))
                     (selected (completing-read "Package: "
                                                table
                                                nil t))
                     (selected-candidate (assoc selected candidates))
                     (urls (nth 2 selected-candidate))
                     files)
                (when (null selected-candidate)
                  (user-error "Invalid candidate"))
                (unwind-protect
                    ;; FIXME this currently doesn't work
                    (let ((reporter (make-progress-reporter "Downloading..." 0 (length urls) 0 1)))
                      (dolist (url urls)
                        (let ((filename (url-file-nondirectory url)))
                          (push filename files)
                          (url-copy-file url filename)
                          (progress-reporter-update reporter )))
                      (let ((tarball-name
                             (concat selected ".tgz")))
                        (ubuntu-source--compress-files tarball-name files)))
                  (mapc (lambda (filename)
                          (ignore-errors
                            (delete-file filename)))
                        files)))))))))

;; (ubuntu-source-find-sources-for-multiple-distros
;;  "curl" (lambda (result)
;;           (message "Result: %S" result)))


(defun ubuntu-source--compress-files (tgz-file file-list)
  "Compress FILE-LIST into TGZ-FILE."
  (with-delayed-message (1 "Compressing...")
    (with-temp-buffer
      (let ((exit-status (apply #'process-file "tar" nil t nil "-caf" tgz-file file-list)))
        (unless (zerop exit-status)
          (delete-file tgz-file)
          (goto-char (point-min))
          (error "tar: %d: %s" exit-status
                 (buffer-substring-no-properties
                  (point-min) (line-end-position))))))))

(provide 'ubuntu-source)
;;; ubuntu-source.el ends here
