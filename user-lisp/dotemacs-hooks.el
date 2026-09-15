;;; dotemacs-hooks.el --- Hooks defined with `hookify!' -*- lexical-binding: t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

;; Author:   Zhengyi Fu <i@fuzy.me>
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0
;; Keywords:

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

;; This module defines hooks that are run by advice installed on
;; existing functions, using the `hookify!' macro from
;; `dotemacs-core'.  Keeping all such hook definitions in one place
;; makes it easy to discover them.

;;; Code:

(eval-when-compile (require 'dotemacs-core))

;;;; Editing

(hookify! recentf-add-file
  :after recentf-add-file-functions
  :args (filename)
  :doc "Abnormal hook run after `recentf-add-file' with FILENAME.")

;;;; Completion

(hookify! embark-dwim
  :before embark-dwim-before-hook
  :doc "Hook run before `embark-dwim'.")

(hookify! marginalia--annotator
  :override marginalia-annotator-function
  :doc "Function used by `marginalia--annotator' to compute the annotator.
Called with the completion category.")

(hookify! consult-dir--bookmark-dirs
  :override consult-dir-bookmark-dirs-function
  :doc "Function used by `consult-dir--bookmark-dirs' to list bookmarked dirs.")

;;;; Programming

(hookify! js-jsx-enable
  :after js-jsx-after-enable-hook
  :doc "Hook run after `js-jsx-enable'.")

(hookify! etags--xref-backend
  :before etags-xref-backend-hook
  :doc "Hook run before `etags--xref-backend' is used.")

(hookify! project-remember-project
  :after project-remember-project-functions
  :args (pr &rest _)
  :doc "Abnormal hook run after `project-remember-project' with the project PR.")

;;;; Org

(hookify! org-protocol-capture
  :before org-protocol-before-capture-hook
  :doc "Hook run before `org-protocol-capture'.")

;;;; Bookmarks

(hookify! bookmark-default-handler
  :after bookmark-default-handler-functions
  :args (record)
  :doc "Abnormal hook run after `bookmark-default-handler' with the bookmark RECORD.")

(hookify! eww-bookmark-jump
  :after eww-bookmark-jump-functions
  :args (record)
  :doc "Abnormal hook run after `eww-bookmark-jump' with the bookmark RECORD.")

(hookify! help-bookmark-jump
  :after help-bookmark-jump-functions
  :args (record)
  :doc "Abnormal hook run after `help-bookmark-jump' with the bookmark RECORD.")

(hookify! xwidget-webkit-bookmark-jump-handler
  :after xwidget-webkit-after-bookmark-jump-hook
  :doc "Hook run after `xwidget-webkit-bookmark-jump-handler'.")

;;;; VC

(hookify! diff-hl-margin-ensure-visible
  :override diff-hl-margin-ensure-visible-function
  :doc "Function used by `diff-hl-margin-ensure-visible'.")

;;;; Misc

(hookify! hack-one-local-variable
  :before hack-one-local-variable-functions
  :args (var val)
  :doc "Abnormal hook run before `hack-one-local-variable' applies VAR and VAL.")

(hookify! proced-format-args
  :override proced-format-args-function
  :doc "Function used by `proced-format-args' to format process arguments.")

(provide 'dotemacs-hooks)
;;; dotemacs-hooks.el ends here
