;; -*- lexical-binding: t; -*-
;; https://www.tencentcloud.com/zh/document/product/436/7778

(require 'auth-source)
(require 'mml)
(require 'url)
(require 'url-http)

(defgroup tencent-cos nil
  "Tencent Cloud Object Storage (COS) integration."
  :tag "Tencent COS"
  :group 'comm)

(defun tencent-cos--binary-to-hex (input)
  (mapconcat (lambda (byte) (format "%02x" byte)) input ""))

(defun tencent-cos--hex-to-binary (input)
  "Convert hex string INPUT to binary data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((i 0)
          (len (length input)))
      (while (< i len)
        (let ((byte (string-to-number (substring input i (+ i 2)) 16)))
          (insert byte))
        (cl-incf i 2)))
    (buffer-string)))

(defun tencent-cos-auth-info (host)
  "Return the first auth-source entry matching HOST (with diminishing sub-domain specificity).
HOST is reduced successively by dropping its left-most label until a
credential set (:user and :secret) is found or HOST becomes empty.  Return
nil when none exists."
  (let (info last-host host-len)
    (setq host-len (length host))
    (while (and (null info) (not (string-empty-p host)))
      (setq last-host host
            host (string-join (cdr (split-string host "\\.")) "."))
      (when (< (length host) host-len)  ;; skip empty replacements
        (setq info (car (auth-source-search
                         :max 1
                         :host last-host
                         :require '(:user :secret))))))
    (unless info
      (warn "No Tencent COS credentials found (tried up from \"%s\")" host))
    info))

(defun tencent-cos--hmac (key data &optional alg)
  "Return MAC for ALG (SHA1 or SHA256)."
  (tencent-cos--binary-to-hex
   (gnutls-hash-mac (or alg "SHA1") key data)))

(cl-defun tencent-cos-sign-request
    (&key secret-id secret-key query-params headers method target
          start-time valid-for)
  "Create Tencent COS V4 query-string authorization signature.

required arguments:
SECRET-ID     – Your Tencent Cloud secret ID (`q-ak`).
SECRET-KEY    – Your Tencent Cloud secret KEY (`q-key`).
QUERY-PARAMS  – Alist of query parameters to be signed (key . value).
HEADERS       – Alist of HTTP header names & values to be signed (key . value).
METHOD        – HTTP method in upper-case, e.g. \"PUT\" or \"GET\".
TARGET        – Absolute URI path (must start with \"/\").

Optional arguments:
START-TIME    – Unix epoch start time for the signature (default: now).
VALID-FOR     – Seconds the signature remains valid (default: 600 → 10 min).

Returns:
A complete query-string ready for use in an Authorization header,
containing `q-sign-algorithm`, `q-ak`, `q-sign-time`, `q-key-time`,
`q-header-list`, `q-url-param-list` and `q-signature` parameters."
  (let* ((start-timestamp (or start-time (time-convert (current-time) 'integer)))
         (end-timestamp (+ start-timestamp (or valid-for 600)))
         (key-time (format "%s;%s" start-timestamp end-timestamp))
         (sign-key (tencent-cos--hmac (substring secret-key) key-time))
         (query-params (mapcar (lambda (elt)
                                 (cons (downcase (url-hexify-string (car elt)))
                                       (url-hexify-string (cdr elt))))
                               query-params))
         (key-list (sort  (map-keys query-params) :lessp #'string<))
         (http-params (mapconcat (lambda (key)
                                   (concat key "=" (map-elt query-params key)))
                                 key-list "&"))
         (url-param-list (string-join key-list ";"))
         (headers (mapcar (lambda (elt)
                            (cons (downcase (url-hexify-string (car elt)))
                                  (url-hexify-string (cdr elt))))
                          headers))
         (key-list (sort (map-keys headers) :lessp #'string<))
         (http-headers (mapconcat (lambda (key)
                                    (concat key "=" (map-elt headers key)))
                                  key-list "&"))
         (header-list (string-join key-list ";"))
         (http-string (concat (downcase method) "\n"
                              target "\n"
                              http-params "\n"
                              http-headers "\n"))
         (string-to-sign (concat "sha1\n"
                                 key-time "\n"
                                 (secure-hash 'sha1 http-string)
                                 "\n"))
         (signature (tencent-cos--hmac (substring sign-key) string-to-sign)))
    (clear-string sign-key)
    ;; (message "http-string: %S" http-string)
    ;; (message "string-to-sign: %S" string-to-sign)
    ;; (message "sign-key: %S" sign-key)
    (concat "q-sign-algorithm=sha1"
            "&q-ak=" secret-id
            "&q-sign-time=" key-time
            "&q-key-time=" key-time
            "&q-header-list=" header-list
            "&q-url-param-list=" url-param-list
            "&q-signature=" signature)))

(defun tencent-cos-host (bucket-appid region)
  (format "%s.cos.%s.myqcloud.com" bucket-appid region))

(defvar url-http-response-status)

(cl-defun tencent-cos-put (&key bucket-appid region file-key
                                      content content-type
                                      content-disposition
                                      secret-id secret-key
                                      callback)
  "Upload content to Tencent Cloud Object Storage (COS).

Required arguments:
BUCKET-APPID: The bucket name with APPID.
REGION: The COS bucket region.
FILE-KEY: The object key (path) in the bucket.
CONTENT: The content to upload.
SECRET-ID: The Tencent Cloud API secret ID.
SECRET-KEY: The Tencent Cloud API secret key.

Optional arguments:
CONTENT-TYPE: The MIME type of the content (defaults to application/octet-stream).
CONTENT-DISPOSITION: The Content-Disposition header value.
CALLBACK: Function to call after upload completion.
         Called with nil on success, error message string on failure."
  (let* ((host (tencent-cos-host bucket-appid region))
         (url-request-method "PUT")
         (url-request-data content)
         (url-request-extra-headers
          `(("Content-Type" . ,(encode-coding-string (or content-type "application/octet-stream") 'utf-8))
            ,@(and content-disposition
                   `(("Content-Disposition" . ,(encode-coding-string content-disposition 'utf-8))))))
         (target (if (string-prefix-p "/" file-key)
                     file-key
                   (concat "/" file-key)))
         (signature (tencent-cos-sign-request
                     :secret-id secret-id
                     :secret-key secret-key
                     :headers `(("host" . ,host)
                                ,@url-request-extra-headers)
                     :method url-request-method
                     :target target))
         (url-request-extra-headers
          `(,@url-request-extra-headers
            ("Authorization" . ,signature)))
         (url (format "https://%s%s" host target)))
    (url-retrieve url
                  (lambda (status)
                    (let ((buffer (current-buffer)))
                      (unwind-protect
                          (cond
                           ;; Handle connection errors
                           ((plist-get status :error)
                            (let ((error-details (plist-get status :error)))
                              (when callback
                                (funcall callback (format "Connection error: %S" error-details)))))
                           ;; Handle HTTP errors
                           ((not (and (>= url-http-response-status 200)
                                      (< url-http-response-status 400)))
                            (goto-char (point-min))
                            (forward-line)
                            (let* ((response-body (buffer-substring (point) (point-max)))
                                   (error-message (format "HTTP %d: %s" url-http-response-status response-body)))
                              (when callback
                                (funcall callback error-message))))
                           ;; Success case
                           (t
                            (when callback
                              (funcall callback nil))))
                        (when (buffer-live-p buffer)
                          (kill-buffer buffer)
                          )))))))

(defcustom tencent-cos-default-link-validity (* 60 60 24)
  "Default validity period for COS download links in seconds."
  :type 'integer
  :group 'tencent-cos)

(defcustom tencent-cos-default-bucket nil
  "Default COS bucket name with APPID."
  :type '(choice (const nil) string)
  :group 'tencent-cos)

(defcustom tencent-cos-default-region nil
  "Default COS bucket region."
  :type '(choice (const nil) string)
  :group 'tencent-cos)

;;;###autoload
(defun tencent-cos-get-new-url (url)
  (interactive "sOld URL: ")
  (let* ((url (url-generic-parse-url url))
         (path-and-query (url-path-and-query url))
         (path (car path-and-query))
         (query (cdr path-and-query))
         (params (url-parse-query-string query))
         (sign-time (car (alist-get "q-sign-time" params nil nil #'equal)))
         (duration (let ((time-range (mapcar #'string-to-number
                                             (split-string sign-time ";"))))
                     (- (nth 1 time-range) (nth 0 time-range))))
         (host (url-host url))
         (secret (tencent-cos-auth-info host))
         (auth-string (tencent-cos-sign-request
                       :secret-id (plist-get secret :user)
                       :secret-key (auth-info-password secret)
                       :headers `(("host" . ,host))
                       :valid-for duration
                       :method "GET"
                       :target path))
         (download-url
          (format "https://%s%s?%s"
                  host path
                  (url-hexify-string
                   auth-string
                   (append '(?% ?& ?=) url-unreserved-chars)))))
    (kill-new download-url)
    (message "New URL copied to the kill ring")))

;;;###autoload
(defun tencent-cos-get-url (file-key duration)
  (interactive
   (list (read-string "File key: ")
         (if current-prefix-arg
             (* 60 60
                (string-to-number
                 (read-string
                  "How many hours will the link be valid for? "
                  nil nil
                  (number-to-string (/ tencent-cos-default-link-validity 3600)))))
           tencent-cos-default-link-validity)))
  (let* ((host (tencent-cos-host tencent-cos-default-bucket
                                 tencent-cos-default-region))
         (secret (tencent-cos-auth-info host))
         (download-url
          (format "https://%s%s?%s"
                  host
                  (url-hexify-string file-key url-path-allowed-chars)
                  (url-hexify-string
                   (tencent-cos-sign-request
                    :secret-id (plist-get secret :user)
                    :secret-key (auth-info-password secret)
                    :headers `(("host" . ,host))
                    :valid-for duration
                    :method "GET"
                    :target file-key)
                   (append '(?% ?& ?=) url-unreserved-chars)))))
    (kill-new download-url)
    (message "URL copied to the kill ring")))

;;;###autoload
(defun tencent-cos-put-buffer (type duration)
  "Upload current buffer contents to Tencent Cloud COS and create a download link.

Upload the buffer contents with content TYPE and generate a signed URL valid
for DURATION seconds. The URL is copied to the kill ring upon completion.

When called interactively:
- TYPE defaults to text/plain for multibyte buffers, application/octet-stream otherwise
- DURATION defaults to `tencent-cos-default-link-validity'
- With prefix arg, prompts for both TYPE and DURATION (in hours)

Uses `tencent-cos-default-bucket' and `tencent-cos-default-region'
for the target bucket configuration.

The file name in COS is generated from the SHA1 hash of the contents
and the buffer name to ensure uniqueness."
  (interactive
   (let* ((type (if enable-multibyte-characters
                    "text/plain; charset=utf-8"
                  "application/octet-stream"))
          (duration tencent-cos-default-link-validity))
     (when current-prefix-arg
       (setq type (mml-minibuffer-read-type (buffer-name) type))
       (setq duration
             (* 60 60
                (string-to-number
                 (read-string
                  "How many hours will the link be valid for? "
                  nil nil
                  (number-to-string (/ tencent-cos-default-link-validity 3600)))))))
     (list type duration)))
  (let* ((contents (encode-coding-string (buffer-string) 'utf-8-unix))
         (file-key (concat "/"
                           (sha1 contents)
                           "-"
                           (buffer-name)))
         (host (tencent-cos-host tencent-cos-default-bucket
                                 tencent-cos-default-region))
         (secret (tencent-cos-auth-info host))
         (buffer (current-buffer))
         (mode-line-process-orig mode-line-process))
    (with-current-buffer buffer
      (setq mode-line-process " Uploading...")
      (force-mode-line-update))
    (tencent-cos-put
     :bucket-appid tencent-cos-default-bucket
     :region tencent-cos-default-region
     :file-key file-key
     :secret-id (plist-get secret :user)
     :secret-key (auth-info-password secret)
     :content contents
     :content-type type
     :content-disposition "inline"
     :callback
     (lambda (error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq mode-line-process mode-line-process-orig)
           (force-mode-line-update)))
       (if error
           (message "Upload failed: %s" error)
         (tencent-cos-get-url file-key duration))))))

;;;###autoload
(defun tencent-cos-put-file (file type duration)
  "Upload file to Tencent Cloud COS and create a download link.

Upload FILE with content TYPE and generate a signed URL valid for
DURATION seconds. The URL is copied to the kill ring upon completion.

When called interactively:
- TYPE is determined from the file name
- DURATION defaults to `tencent-cos-default-link-validity'
- With prefix arg, prompts for DURATION (in hours)"
  (interactive
   (let* ((file (mml-minibuffer-read-file "File: "))
          (type (mml-minibuffer-read-type file))
          (duration tencent-cos-default-link-validity))
     (when current-prefix-arg
       (setq duration
             (* 60 60
                (string-to-number
                 (read-string
                  "How many hours will the link be valid for? "
                  nil nil
                  (number-to-string (/ tencent-cos-default-link-validity 3600)))))))
     (list file type duration)))
  (let* ((contents (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally file)
                     (buffer-string)))
         (file-key (format "/%s-%s"
                           (sha1 contents)
                           (file-name-nondirectory file)))
         (host (tencent-cos-host tencent-cos-default-bucket tencent-cos-default-region))
         (secret (tencent-cos-auth-info host))
         (buffer (current-buffer))
         (mode-line-process-orig mode-line-process))
    (with-current-buffer buffer
      (setq mode-line-process " Uploading...")
      (force-mode-line-update))
    (tencent-cos-put
     :bucket-appid tencent-cos-default-bucket
     :region tencent-cos-default-region
     :file-key file-key
     :secret-id (plist-get secret :user)
     :secret-key (auth-info-password secret)
     :content contents
     :content-type type
     :content-disposition (format "attachment; filename=\"%s\""
                                  (url-hexify-string
                                   (file-name-nondirectory file)))
     :callback
     (lambda (error)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq mode-line-process mode-line-process-orig)
           (force-mode-line-update)))
       (if error
           (message "Upload failed: %s" error)
         (tencent-cos-get-url file-key duration))))))

(provide 'tencent-cos)
;;; tencent-cos.el ends here
