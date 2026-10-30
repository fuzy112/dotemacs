;;; herdr-opencode.el --- OpenCode question provider for Herdr -*- lexical-binding: t; -*-

;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

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

;; OpenCode-specific question provider for `herdr'.  OpenCode
;; (<https://opencode.ai>) stores its message history in a SQLite
;; database, so question prompts are read from there instead of
;; scraping the alternate-screen TUI, which is unreliable mid-redraw.
;; This file implements `herdr-question-fetch-function' and
;; `herdr-question-status-function' and registers them with `herdr'.

;;; Code:

(require 'cl-lib)
(require 'herdr)
(require 'sqlite)
(require 'xdg)

(defcustom herdr-opencode-db
  (expand-file-name "opencode/opencode.db" (xdg-data-home))
  "Path to OpenCode's SQLite message database.
OpenCode stores data under \"${XDG_DATA_HOME:-~/.local/share}/opencode\".
Question prompts are read from here instead of scraping the
alternate-screen TUI, which is unreliable mid-redraw."
  :type 'file
  :group 'herdr)

(defconst herdr-opencode--schema
  '(("session_v2" . ("id" "directory"))
    ("session_message" . ("data" "session_id" "type" "seq")))
  "OpenCode tables and columns this provider depends on.")

(defvar herdr-opencode--schema-ok nil
  "List of DB files whose schema has already been validated.")

(defun herdr-opencode--db-check-schema (db file)
  "Signal a descriptive error if DB (at FILE) lacks the expected schema."
  (dolist (spec herdr-opencode--schema)
    (let* ((table (car spec))
           (wanted (cdr spec))
           (info (sqlite-select db (format "PRAGMA table_info(%s)" table)))
           (cols (mapcar #'cadr info)))
      (unless info
        (error "herdr-opencode: %s has no %s table" file table))
      (dolist (col wanted)
        (unless (member col cols)
          (error "herdr-opencode: %s.%s lacks column %s"
                 file table col))))))

(defun herdr-opencode--db-open ()
  "Return a SQLite handle for OpenCode's DB, or nil.
Signal an error if the database exists but lacks the expected schema;
the schema is validated once per file and cached."
  (when-let* ((file (expand-file-name herdr-opencode-db))
              ((file-exists-p file))
              (db (ignore-errors (sqlite-open file))))
    (unless (member file herdr-opencode--schema-ok)
      (herdr-opencode--db-check-schema db file)
      (push file herdr-opencode--schema-ok))
    db))

(defmacro herdr-opencode--with-db (var &rest body)
  "Evaluate BODY with VAR bound to OpenCode's SQLite database.
If the database is unavailable, skip BODY and return nil.  The
database is closed when BODY finishes, even on error."
  (declare (indent 1) (debug (symbolp body)))
  `(when-let* ((,var (herdr-opencode--db-open)))
     (unwind-protect
         (progn ,@body)
       (sqlite-close ,var))))

(defun herdr-opencode--message-tool-part (message-obj tool-name)
  "Return the first TOOL-NAME tool part in MESSAGE-OBJ, or nil.
MESSAGE-OBJ is a JSON message hash table with a `content' array."
  (cl-loop for part across (and message-obj (gethash "content" message-obj))
           when (and (hash-table-p part)
                     (equal (gethash "type" part) "tool")
                     (equal (gethash "name" part) tool-name))
           return part))

(defun herdr-opencode--recent-question-parts (db session-id)
  "Return recent question tool parts for SESSION-ID from DB, newest first."
  (cl-loop for row in (sqlite-select
                       db
                       (concat "SELECT data FROM session_message"
                               " WHERE session_id = ? AND type = 'assistant'"
                               " ORDER BY seq DESC LIMIT 40")
                       (list session-id))
           for part = (herdr-opencode--message-tool-part
                       (json-parse-string (car row)) "question")
           when part
           collect part))

(defun herdr-opencode--question-find (directory)
  "Return the running question tool call for DIRECTORY, or nil.
The result is a plist (:session-id SID :tool-id TID :questions VECTOR)
where VECTOR is the raw JSON array of question objects."
  (herdr-opencode--with-db db
    (cl-loop for row in (sqlite-select db
                                       "SELECT id FROM session_v2 WHERE directory = ?"
                                       (list directory))
             for sid = (car row)
             for part = (cl-loop for candidate
                                 in (herdr-opencode--recent-question-parts db sid)
                                 when (equal (gethash "status"
                                                      (gethash "state" candidate))
                                             "running")
                                 return candidate)
             when part
             return (let* ((state (gethash "state" part))
                           (input (gethash "input" state)))
                      (list :session-id sid
                            :tool-id (gethash "id" part)
                            :questions (gethash "questions" input))))))

;;;###autoload
(defun herdr-opencode--question-status (session-id tool-id)
  "Return the status of question tool TOOL-ID in SESSION-ID, or nil."
  (herdr-opencode--with-db db
    (cl-loop for part in (herdr-opencode--recent-question-parts db session-id)
             when (equal (gethash "id" part) tool-id)
             return (gethash "status" (gethash "state" part)))))

(defun herdr-opencode--question-plist (question-ht)
  "Convert QUESTION-HT, a DB question hash table, into a display plist.
The result has :header, :text, :multiple, and :options, where each
option is a plist with :label and :description."
  (let ((options (gethash "options" question-ht)))
    (list :header (gethash "header" question-ht)
          :text (gethash "question" question-ht)
          :multiple (eq (gethash "multiple" question-ht) t)
          :options (cl-loop for option across options
                            collect (list :label (gethash "label" option)
                                          :description
                                          (gethash "description" option))))))

;;;###autoload
(defun herdr-opencode--question-fetch (agent)
  "Return AGENT's pending question form, or nil.
The form is (:session-id SID :tool-id TID :questions (Q...)) where each
Q is a display plist with :header, :text, and :options."
  (when-let* ((found (herdr-opencode--question-find (plist-get agent :cwd))))
    (list :session-id (plist-get found :session-id)
          :tool-id (plist-get found :tool-id)
          :questions (cl-loop for question across (plist-get found :questions)
                              collect (herdr-opencode--question-plist
                                       question)))))

;;;###autoload
(add-hook 'herdr-question-fetch-functions #'herdr-opencode--question-fetch)
;;;###autoload
(add-hook 'herdr-question-status-functions #'herdr-opencode--question-status)

(provide 'herdr-opencode)

;;; herdr-opencode.el ends here
