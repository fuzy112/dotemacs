;;; bangs.el --- Kagi bangs for Emacs -*- lexical-binding: t -*-
;; Copyright (C) 2026 Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, hypermedia, web

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package integrates Kagi bangs into Emacs.
;;
;; Usage:
;;   (require 'bangs)
;;   (bangs-global-mode)            ; Enable !bang in browse-url
;;   (keymap-global-set "C-c !" #'bangs)
;;
;; Commands:
;;   - M-x bangs: Select trigger with completion
;;   - C-u M-x bangs: Type "!w emacs" at once
;;
;; Global minor mode:
;;   - M-x bangs-global-mode: Toggle !bang transformation in browse-url

;;; Code:

(require 'json)
(require 'url)
(require 'browse-url)
(require 'cl-lib)

(defgroup bangs nil
  "Kagi bangs for Emacs."
  :prefix "bangs-"
  :group 'browse-url)

(cl-defstruct bangs-data
  "Structure holding bang information."
  (name nil :documentation "Display name")
  (base-url nil :documentation "Base URL or full URL template for regex-based")
  (prefix nil :documentation "URL prefix before query (nil for regex-based)")
  (suffix nil :documentation "URL suffix after query (nil for regex-based)")
  (regex nil :documentation "Regex pattern for query parsing (nil for simple)")
  (fmt nil :documentation "Format specifiers (nil for simple)"))

(defcustom bangs-urls
  '("https://raw.githubusercontent.com/kagisearch/bangs/main/data/bangs.json"
    "https://raw.githubusercontent.com/kagisearch/bangs/main/data/kagi_bangs.json")
  "List of URLs to download the Kagi bangs database from.
Each URL should point to a JSON file containing bang definitions."
  :type '(repeat string))

(defcustom bangs-cache-file
  (expand-file-name "bangs.json" user-emacs-directory)
  "Path to the local cache file for Kagi bangs JSON data."
  :type 'file)

(defcustom bangs-fallback-base-url
  "https://www.kagi.com"
  "Base URL to use for bang templates without a scheme.
Some bang templates are relative URLs without http:// prefix.
This URL is prepended to such templates.

IMPORTANT: This variable must be set before loading the bangs package,
before parsing templates that lack a scheme."
  :type 'string)

(defcustom bangs-user-bangs
  nil
  "List of user-defined bangs.
Each element is a list. There are two formats:

1. Simple bang with {{{s}}} placeholder:
   (TRIGGER NAME URL-TEMPLATE &rest SECONDARY-TRIGGERS)
   Example: (\"mysearch\" \"My Search\" \"https://example.com?q={{{s}}}\")

2. Regex-based bang with $1, $2, etc.:
   (TRIGGER NAME URL-TEMPLATE :regex REGEX [:fmt FMT] &rest SECONDARY-TRIGGERS)
   Example: (\"translate\" \"Translate\" \"https://example.com/$1/$2\"
             :regex \"(\\w+)\\s+(.*)\" \"en\" \"es\")

For regex-based bangs, REGEX is used to split the query into groups
that replace $1, $2, etc. in URL-TEMPLATE. The optional FMT is a
list of format specifiers (currently only `url_encode_placeholder'
is supported, which is the default behavior)."
  :type '(repeat (list :tag "Bang"
                       (string :tag "Trigger")
                       (string :tag "Name")
                       (string :tag "URL Template")
                       (repeat :tag "Additional Arguments (keywords or triggers)" sexp))))

(defun bangs--minibuffer-setup-default ()
  "Default minibuffer setup function that adds completion-at-point for bangs."
  (add-hook 'completion-at-point-functions #'bangs--completion-at-point nil t))

(defcustom bangs-minibuffer-setup-function
  #'bangs--minibuffer-setup-default
  "Function to run in minibuffer setup when using `bangs' with prefix arg.
This function is called with no arguments in the minibuffer context when
reading the bang input string with `C-u M-x bangs'.  The default value
adds completion-at-point for bang triggers.

Other valid usage of this hook could be enabling alternative completion
UI, such as Corfu and Company.

Example:

  (lambda ()
    (bangs--minibuffer-setup-default)
    (corfu-mode)
    (setq-local corfu-auto t
                corfu-auto-trigger \"!\"))
"
  :type 'function)

(defvar bangs--table nil
  "Hash table mapping triggers to bangs-data structs.
Each value is a `bangs-data' struct.
REGEX is the regex pattern for splitting query (nil for simple bangs).
FMT is the format list for encoding (nil for simple bangs).")

(defvar bangs--cache-loaded nil
  "Non-nil if bangs data has been loaded into memory.")

(defun bangs--vector-to-list (data)
  "Convert DATA to a list if it's a vector, otherwise return as-is."
  (if (vectorp data) (append data nil) data))

(defun bangs--download-single-url (url)
  "Download bangs data from a single URL.
Returns the parsed JSON data or signals an error."
  (message "Downloading from %s..." url)
  (let ((buffer (url-retrieve-synchronously url)))
    (if (not buffer)
        (error "Failed to download from %s" url)
      (with-current-buffer buffer
        (goto-char (point-min))
        (unless (search-forward "\n\n" nil t)
          (kill-buffer buffer)
          (error "Invalid HTTP response from %s" url))
        (let ((content (buffer-substring (point) (point-max))))
          (kill-buffer buffer)
          (condition-case nil
              (json-read-from-string content)
            (json-error
             (error "Invalid JSON response from %s" url))))))))

(defun bangs--download-cache ()
  "Download the Kagi bangs database from all URLs and save to cache file.
Data from multiple sources is merged into a single list."
  (message "Downloading Kagi bangs from %d source(s)..." (length bangs-urls))
  (let ((all-bangs nil))
    (dolist (url bangs-urls)
      (condition-case err
          (let ((data (bangs--download-single-url url)))
            (setq all-bangs (append all-bangs (bangs--vector-to-list data))))
        (error (message "Warning: Failed to download from %s: %s"
                        url (error-message-string err)))))
    (if (null all-bangs)
        (error "Failed to download bangs data from any source")
      ;; Ensure parent directory exists before writing
      (let ((cache-dir (file-name-directory bangs-cache-file)))
        (unless (file-directory-p cache-dir)
          (make-directory cache-dir t)))
      (with-temp-buffer
        (insert (json-encode all-bangs))
        (let ((coding-system-for-write 'utf-8))
          (write-region (point-min) (point-max) bangs-cache-file nil 'quiet)))
      (bangs-clear-cache)
      (message "Kagi bangs cached to %s (%d bangs)"
               bangs-cache-file (length all-bangs)))))

;;;###autoload
(defun bangs-clear-cache ()
  "Clear the in-memory bangs cache."
  (interactive)
  (setq bangs--table nil
        bangs--cache-loaded nil)
  (message "Bangs cache cleared"))

;;;###autoload
(defun bangs-update-cache ()
  "Force re-download of the Kagi bangs database."
  (interactive)
  (bangs--download-cache))

(defun bangs--ensure-cache ()
  "Ensure bangs cache file exists, downloading if necessary."
  (unless (file-exists-p bangs-cache-file)
    (bangs--download-cache)))

(defun bangs--parse-url-template (url-template &optional regex fmt)
  "Parse a Kagi URL TEMPLATE and return a `bangs-data' struct.
For simple templates with {{{s}}}, returns a struct with PREFIX and SUFFIX set.
For regex-based templates (with $1, $2, etc.), returns a struct with REGEX set.

Note: The name field is not set; callers must set it separately."
  (let* ((placeholder "{{{s}}}")
         (has-regex-placeholder (and regex (string-match "\\$[0-9]+" url-template)))
         (url-template
          (if (string-match "\\`https?://" url-template)
              url-template
            (concat bangs-fallback-base-url url-template)))
         (pos (string-match (regexp-quote placeholder) url-template)))
    (cond
     ;; Regex-based template with $1, $2, etc.
     (has-regex-placeholder
      (make-bangs-data :base-url url-template :regex regex :fmt fmt))
     ;; Simple template with {{{s}}}
     (pos
      (let* ((prefix (substring url-template 0 pos))
             (suffix (substring url-template (+ pos (length placeholder))))
             (base-url (or (and (string-match "\\`\\(https?://[^/]+\\)" url-template)
                                (match-string 1 url-template))
                           (and (string-match "\\`\\(https?://[^/]+\\)" prefix)
                                (match-string 1 prefix))
                           prefix)))
        (make-bangs-data :base-url base-url :prefix prefix :suffix suffix)))
     ;; No placeholder - just return the URL as base
     (t
      (make-bangs-data :base-url url-template :prefix url-template :suffix "")))))

(defun bangs--insert-bang (trigger name url-template secondary-triggers regex fmt)
  "Insert a single bang into the hash table.
TRIGGER is the primary trigger. NAME is the display name.
URL-TEMPLATE is the URL template. SECONDARY-TRIGGERS is a list of
alternative triggers. REGEX is the regex pattern (or nil). FMT is
a list of format specifiers (or nil)."
  (let ((data (bangs--parse-url-template url-template regex fmt)))
    ;; Set the name field
    (setf (bangs-data-name data) name)
    (puthash trigger data bangs--table)
    (when secondary-triggers
      (dolist (st secondary-triggers)
        (puthash st data bangs--table)))))

(defun bangs--load-user-bangs ()
  "Load user-defined bangs into the hash table.
User bangs override downloaded bangs if there are trigger conflicts."
  (dolist (bang bangs-user-bangs)
    (when (>= (length bang) 3)
      (let ((trigger (nth 0 bang))
            (name (nth 1 bang))
            (url-template (nth 2 bang))
            (regex nil)
            (fmt nil)
            (secondary-triggers nil)
            (rest (nthcdr 3 bang)))
        ;; Parse optional :regex and :fmt keywords
        (while rest
          (cond
           ((eq (car rest) :regex)
            (setq regex (cadr rest)
                  rest (cddr rest)))
           ((eq (car rest) :fmt)
            (setq fmt (if (listp (cadr rest))
                          (cadr rest)
                        (list (cadr rest)))
                  rest (cddr rest)))
           (t
            ;; Must be a secondary trigger
            (push (car rest) secondary-triggers)
            (setq rest (cdr rest)))))
        (setq secondary-triggers (nreverse secondary-triggers))
        (bangs--insert-bang trigger name url-template secondary-triggers regex fmt)))))

(defun bangs--load-data ()
  "Load bangs data from cache file and user-defined bangs into hash table.
User-defined bangs override downloaded bangs if there are trigger conflicts."
  (bangs--ensure-cache)
  (unless bangs--cache-loaded
    (setq bangs--table (make-hash-table :test 'equal))
    ;; Load downloaded bangs
    (let ((bangs (json-read-file bangs-cache-file)))
      (dolist (bang (bangs--vector-to-list bangs))
        (let* ((name (cdr (assq 's bang)))
               (trigger (cdr (assq 't bang)))
               (url-template (cdr (assq 'u bang)))
               (secondary-triggers (cdr (assq 'ts bang)))
               (regex (cdr (assq 'x bang)))
               (fmt (cdr (assq 'fmt bang)))
               (fmt-list (when fmt
                           (if (listp fmt) fmt (bangs--vector-to-list fmt))))
               (secondary-list (when secondary-triggers
                                 (if (listp secondary-triggers)
                                     secondary-triggers
                                   (bangs--vector-to-list secondary-triggers)))))
          (bangs--insert-bang trigger name url-template secondary-list regex fmt-list))))
    ;; Load user-defined bangs (override downloaded ones)
    (bangs--load-user-bangs)
    (setq bangs--cache-loaded t)))

(defun bangs--get-data (trigger)
  "Get bang data for TRIGGER."
  (bangs--load-data)
  (gethash trigger bangs--table))

(defun bangs--build-url-with-regex (url-template regex query)
  "Build URL using regex to split QUERY and replace $1, $2, etc. in URL-TEMPLATE.
REGEX is used to match and capture groups from QUERY.
Returns URL-TEMPLATE with $N replaced by captured groups (URL-encoded)."
  (if (or (null query) (string= query ""))
      url-template
    (if (not (string-match regex query))
        ;; Query doesn't match regex, use as-is with URL encoding
        (replace-regexp-in-string "\\$[0-9]+" (url-encode-url query) url-template t t)
      ;; Query matches regex, replace $N with captured groups
      (let ((result url-template)
            (max-groups (/ (length (match-data)) 2)))
        (dotimes (i max-groups)
          (let ((group-num (1+ i)))
            (when (match-beginning group-num)
              (let ((group-value (match-string group-num query)))
                (setq result (replace-regexp-in-string
                              (format "\\$%d" group-num)
                              (url-encode-url group-value)
                              result t t))))))
        result))))

(defun bangs--build-url (trigger query)
  "Build URL from TRIGGER and QUERY.
Handles both simple bangs ({{{s}}} placeholder)
and regex-based bangs ($1, $2, etc.)."
  (let ((data (bangs--get-data trigger)))
    (if (not data)
        (error "Unknown bang: %s" trigger)
      (let ((base-url (bangs-data-base-url data))
            (prefix (bangs-data-prefix data))
            (suffix (bangs-data-suffix data))
            (regex (bangs-data-regex data)))
        (if regex
            ;; Regex-based bang (e.g., !ktr)
            (bangs--build-url-with-regex base-url regex query)
          ;; Simple bang with {{{s}}} placeholder
          (if (or (null query) (string= query ""))
              base-url
            (concat prefix (url-encode-url query) suffix)))))))

(defun bangs--affixation-function (candidates)
  "Return affixes for CANDIDATES showing the site name."
  (mapcar (lambda (cand)
            (let ((data (bangs--get-data cand)))
              (list cand
                    (propertize "!" 'face 'shadow)
                    (concat (propertize " " 'display '(space :align-to 20))
                            (propertize (bangs-data-name data) 'face 'completions-annotations)))))
          candidates))

(define-completion-category 'bang nil
  "Completion category for completing bangs (!)."
  :styles '(substring basic))

(defvar bangs--completion-metadata
  `((category . bang)        ; categories are usually in singular form
    (affixation-function . ,#'bangs--affixation-function)
    (cycle-sort-function . ,#'identity)
    (display-sort-function . ,#'identity)))

(defun bangs--completion-table ()
  (bangs--load-data)
  (let ((table bangs--table))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata . ,bangs--completion-metadata)
        (complete-with-action action table str pred)))))

(defun bangs--completing-read-trigger ()
  "Read a bang trigger with completion and annotations."
  (completing-read "Bang: " (bangs--completion-table) nil t))

(defun bangs--completion-at-point ()
  (when-let* ((beg (and (looking-back "![a-zA-Z0-9._-]*" (pos-bol))
                        (match-beginning 0)))
              (end (point))
              (table (bangs--completion-table)))
    `( ,(1+ beg) ,end
       ,table
       :exclusive no
       :exit-function ,(lambda (_str status)
                         (unless (eq status 'exact)
                           (insert " "))))))

(defun bangs--parse-input (input)
  "Parse input like `!w emacs' or `w! emacs' into (TRIGGER . QUERY)."
  (when (string-match "\\`\\(![a-zA-Z0-9._-]+\\|[a-zA-Z0-9._-]+!\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'" input)
    (let* ((match (match-string 1 input))
           (query (match-string 2 input))
           (trigger (if (string-prefix-p "!" match)
                        (substring match 1)
                      (substring match 0 -1))))
      (cons trigger query))))

;;;###autoload
(defun bangs (&optional arg)
  "Use Kagi bangs to search websites.
Without prefix ARG: prompt for trigger with completion, then query.
With prefix ARG: read entire input like '!w emacs'."
  (interactive "P")
  (if arg
      (let* ((input (minibuffer-with-setup-hook
                        bangs-minibuffer-setup-function
                      (read-string "Bang with query: ")))
             (parsed (bangs--parse-input input)))
        (if (not parsed)
            (message "Invalid bang format. Use: !w emacs or w! emacs")
          (let* ((trigger (car parsed))
                 (query (cdr parsed))
                 (url (bangs--build-url trigger query)))
            (browse-url url))))
    (let* ((trigger (bangs--completing-read-trigger))
           (query (read-string (format "%s query: " trigger)))
           (url (bangs--build-url trigger query)))
      (browse-url url))))

(defun bangs--transform-url (url)
  "Transform bang patterns to actual URLs.
Called via advice on `browse-url'."
  (if-let* ((parsed (bangs--parse-input url)))
      (let ((trigger (car parsed))
            (query (cdr parsed)))
        (if (bangs--get-data trigger)
            (bangs--build-url trigger query)
          url))
    url))

(defun bangs--browse-url-advice (args)
  "Advice for `browse-url' to transform bang patterns.
Transforms the URL in ARGS if it matches a bang pattern."
  ;; This interactive form replaces the one in the original function,
  ;; even if this is a :filter-args advice.
  (interactive
   (minibuffer-with-setup-hook
       bangs-minibuffer-setup-function
     (browse-url-interactive-arg "URL: ")))
  (cons (bangs--transform-url (car args)) (cdr args)))

;;;###autoload
(define-minor-mode bangs-global-mode
  "Global minor mode to enable Kagi bangs in `browse-url'.
When enabled, !bang patterns like \"!w emacs\" are automatically
transformed to search URLs before opening.

You can also use the `bangs' command for interactive completion."
  :global t
  (advice-remove 'browse-url #'bangs--browse-url-advice)
  (when bangs-global-mode
    (advice-add 'browse-url :filter-args #'bangs--browse-url-advice)))

(provide 'bangs)
;;; bangs.el ends here
