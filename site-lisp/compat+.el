;;; compat+.el --- Compatibility utility             -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Zhengyi Fu

;; Author: Zhengyi Fu <i@fuzy.me>
;; Keywords: lisp, local

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

;; This package contains constructs backported from Emacs-master that
;; are not currently included in compat.el.

;;; Code:


;;;###autoload
(unless (fboundp 'with-work-buffer)
  (defalias 'with-work-buffer 'with-temp-buffer))

(provide 'compat+)
;;; compat+.el ends here
