;;; debbugs-tests.el --- tests for debbugs.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Morgan Smith <Morgan.J.Smith@outlook.com>
;; Package: debbugs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Please ensure tests don't actually make network calls.

;;; Code:

(require 'debbugs)

;;; Helper Data:

;; Generated using this:
;; (soap-invoke debbugs-wsdl debbugs-port "get_status" [64064])
(defconst debbugs-test--bug-status-soap-return
  '(((item
      (key . 64064)
      (value
       (package . "emacs") (found_date) (last_modified . 1689593050)
       (affects) (date . 1686745022) (fixed_versions)
       (originator . "Morgan Smith <Morgan.J.Smith@outlook.com>")
       (blocks) (archived . 1) (found) (unarchived) (tags . "patch")
       (severity . "normal") (location . "archive") (owner) (fixed)
       (blockedby) (pending . "done") (keywords . "patch") (id . 64064)
       (found_versions) (mergedwith) (summary) (forwarded)
       (log_modified . 1689593050)
       (done . "Michael Albinus <michael.albinus@gmx.de>")
       (source . "unknown")
       (msgid
        . "<DM5PR03MB31632E3A4FE170C62E7D4B0CC55AA@DM5PR03MB3163.namprd03.prod.outlook.com>")
       (bug_num . 64064) (subject . "[PATCH 0/4] debbugs improvements")
       (fixed_date)))))
  "Mock result from `soap-invoke' for bug 64064.")

;; Generated using this:
;; (debbugs-get-status 64064)
(defconst debbugs-test--bug-status
  '(((cache_time . 5000) (source . "unknown") (unarchived)
     (keywords "patch") (blocks) (pending . "done") (severity . "normal")
     (done . "Michael Albinus <michael.albinus@gmx.de>") (location . "archive")
     (log_modified . 1689593050) (subject . "[PATCH 0/4] debbugs improvements")
     (last_modified . 1689593050) (found) (tags "patch") (package "emacs")
     (originator . "Morgan Smith <Morgan.J.Smith@outlook.com>") (archived . t)
     (blockedby) (affects) (mergedwith) (summary) (date . 1686745022)
     (fixed_versions) (id . 64064) (fixed) (found_date) (forwarded)
     (msgid
      . "<DM5PR03MB31632E3A4FE170C62E7D4B0CC55AA@DM5PR03MB3163.namprd03.prod.outlook.com>")
     (owner) (found_versions) (fixed_date) (bug_num . 64064)))
  "Mock result from `debbugs-get-status' for bug 64064.")

;;; Helper Functions:

(defvar debbugs-test--soap-operation-name nil)
(defvar debbugs-test--soap-parameters nil)
(defun debbugs-test--soap-invoke-internal
    (callback _cbargs _wsdl _service operation-name &rest parameters)
  "Over-ride for testing."
  (setq debbugs-test--soap-operation-name operation-name)
  (setq debbugs-test--soap-parameters parameters)
  (let ((return
         (cond ((string-equal operation-name "get_status")
                debbugs-test--bug-status-soap-return)
               ((string-equal operation-name "get_usertag")
                '(((hi))))
               (t '((0))))))
    (if callback
        (progn
          (funcall callback return)
          nil)
      return)))

(add-function
 :override (symbol-function #'soap-invoke-internal)
 #'debbugs-test--soap-invoke-internal)

;;; Tests:

(ert-deftest debbugs-test-get-bugs ()
  "Test \"get_bugs\"."
  (let (debbugs-test--soap-operation-name debbugs-test--soap-parameters)
    (debbugs-get-bugs
     :tag "patch"
     :severity "critical"
     :status "open"
     :status "forwarded")
    (should (string-equal debbugs-test--soap-operation-name "get_bugs"))
    (should (equal debbugs-test--soap-parameters
                   '(["tag" "patch" "severity" "critical"
                      "status" "open" "status" "forwarded"])))))

(ert-deftest debbugs-test-newest-bugs ()
  "Test \"newest_bugs\"."
  (let (debbugs-test--soap-operation-name debbugs-test--soap-parameters)
    (debbugs-newest-bugs 4)
    (should (string-equal debbugs-test--soap-operation-name "newest_bugs"))
    (should (equal debbugs-test--soap-parameters '(4)))))

(ert-deftest debbugs-test-get-status ()
  "Test \"get_status\"."
  (let (debbugs-test--soap-operation-name debbugs-test--soap-parameters)
    (cl-letf (((symbol-function #'float-time)
               (lambda (&optional _specified-time) 5000)))
      (should (= (float-time) 5000))
      (should (equal (sort (car (debbugs-get-status 64064)))
                     (sort (car debbugs-test--bug-status))))
      (should (string-equal debbugs-test--soap-operation-name "get_status"))
      (should (equal debbugs-test--soap-parameters '([64064]))))))

(ert-deftest debbugs-test-get-usertag ()
  "Test \"get_usertag\"."
  (let (debbugs-test--soap-operation-name debbugs-test--soap-parameters)
    (should (equal (debbugs-get-usertag :user "emacs") '("hi")))
    (should (string-equal debbugs-test--soap-operation-name "get_usertag"))
    (should (equal debbugs-test--soap-parameters '("emacs")))))

(provide 'debbugs-tests)

;;; debbugs-tests.el ends here
