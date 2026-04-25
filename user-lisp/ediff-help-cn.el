;;; ediff-help-cn.el --- Chinese help messages for Ediff  -*- lexical-binding:t -*-
;; Copyright © 2026  Zhengyi Fu <i@fuzy.me>

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; English:
;;   Provides a Chinese translation of Ediff help messages.
;;   After loading this file, call `ediff-enable-chinese-help' to enable
;;   Chinese help, or `ediff-disable-chinese-help' to restore English help.
;;   You can also set `ediff-long-help-message-function' and
;;   `ediff-brief-help-message-function' to the functions provided below.
;;
;; 中文：
;;   提供 Ediff 帮助信息的中文版本。
;;   加载本文件后，调用 `ediff-enable-chinese-help' 启用中文帮助，
;;   调用 `ediff-disable-chinese-help' 恢复英文帮助；
;;   或手动将 `ediff-long-help-message-function' 和
;;   `ediff-brief-help-message-function' 设置为下文提供的函数。

;;; Code:

(require 'ediff-init)

(defconst ediff-long-help-message-head-cn
  "    移动浏览        |      切换功能             |        操作处理
=====================|===========================|============================="
  "Header line for the full Chinese help message.")

(defconst ediff-long-help-message-tail-cn
  "=====================|===========================|=============================
    R -显示注册表    |     = -比较区域           |  M   -显示会话组
    D -diff 输出     |     E -浏览 Ediff 手册    |  G   -发送错误报告
    i -状态信息      |     ? -关闭帮助           |  z/q -挂起/退出
-------------------------------------------------------------------------------
查看特定命令的详细帮助：用鼠标中键点击该命令；或将光标移到该命令上按 RET 键。"
  "Tail of the full Chinese help message.")

(defconst ediff-long-help-message-compare3-cn
  "
p,DEL -上一个差异  |     | -垂直/水平分屏       | xy -将缓冲区 X 的区域复制到 Y
n,SPC -下一个差异  |     h -高亮显示            | rx -恢复缓冲区 X 的旧差异
    j -跳转到差异  |     @ -自动细化            |  * -细化当前区域
   gx -转到 X 的光标|    ## -忽略空白           |  ! -更新差异区域
  C-l -重新居中    |    #c -忽略大小写         |
  v/V -向上/下滚动 | #f/#h -聚焦/隐藏区域      | wx -保存缓冲区 X
  </> -向左/右滚动 |     X -设置缓冲区 X 只读  | wd -保存 diff 输出
    ~ -轮换缓冲区  |     m -宽屏显示           |
"
  "Chinese help message normally used for 3-way comparison.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-compare2-cn
  "
p,DEL -上一个差异  |     | -垂直/水平分屏       |a/b -将 A/B 的区域复制到 B/A
n,SPC -下一个差异  |     h -高亮显示            | rx -恢复缓冲区 X 的旧差异
    j -跳转到差异  |     @ -自动细化            |  * -细化当前区域
   gx -转到 X 的光标|    ## -忽略空白           |  ! -更新差异区域
  C-l -重新居中    |    #c -忽略大小写         |
  v/V -向上/下滚动 | #f/#h -聚焦/隐藏区域      | wx -保存缓冲区 X
  </> -向左/右滚动 |     X -设置缓冲区 X 只读  | wd -保存 diff 输出
    ~ -交换变体    |     m -宽屏显示           |
"
  "Chinese help message normally used for 2-way comparison.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-narrow2-cn
  "
p,DEL -上一个差异  |     | -垂直/水平分屏       |a/b -将 A/B 的区域复制到 B/A
n,SPC -下一个差异  |     h -高亮显示            | rx -恢复缓冲区 X 的旧差异
    j -跳转到差异  |     @ -自动细化            |  * -细化当前区域
   gx -转到 X 的光标|    ## -忽略空白           |  ! -更新差异区域
  C-l -重新居中    |    #c -忽略大小写         |  % -窄化/展宽缓冲区
  v/V -向上/下滚动 | #f/#h -聚焦/隐藏区域      | wx -保存缓冲区 X
  </> -向左/右滚动 |     X -设置缓冲区 X 只读  | wd -保存 diff 输出
    ~ -交换变体    |     m -宽屏显示           |
"
  "Chinese help message when comparing windows or regions line-by-line.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-word-mode-cn
  "
p,DEL -上一个差异  |     | -垂直/水平分屏       | xy -将缓冲区 X 的区域复制到 Y
n,SPC -下一个差异  |     h -高亮显示            | rx -恢复缓冲区 X 的旧差异
    j -跳转到差异  |                           |
   gx -转到 X 的光标|    % -窄化/展宽缓冲区     |  ! -重新计算差异
  C-l -重新居中    |    #c -忽略大小写         |
  v/V -向上/下滚动 | #f/#h -聚焦/隐藏区域      | wx -保存缓冲区 X
  </> -向左/右滚动 |     X -设置缓冲区 X 只读  | wd -保存 diff 输出
    ~ -交换变体    |     m -宽屏显示           |
"
  "Chinese help message when comparing windows or regions word-by-word.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-merge-cn
  "
p,DEL -上一个差异  |     | -垂直/水平分屏       |  x -将缓冲区 X 的区域复制到 C
n,SPC -下一个差异  |     h -高亮显示            |  r -恢复缓冲区 C 的旧差异
    j -跳转到差异  |     @ -自动细化            |  * -细化当前区域
   gx -转到 X 的光标|    ## -忽略空白           |  ! -更新差异区域
  C-l -重新居中    | #f/#h -聚焦/隐藏区域      |  + -合并差异区域
  v/V -向上/下滚动 |     X -设置缓冲区 X 只读  | wx -保存缓冲区 X
  </> -向左/右滚动 |     m -宽屏显示           | wd -保存 diff 输出
    ~ -交换变体    |     s -收缩缓冲区 C       |  / -显示/隐藏祖先缓冲区
                     |  $$ -仅显示冲突         |  & -使用新默认值合并
                     |  $* -跳过已更改区域     |
"
  "Chinese help message for merge sessions.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-brief-message-string-cn
  " 按 ? 查看帮助"
  "Contents of the brief Chinese help message.")

;;;###autoload
(defun ediff-long-help-message-cn ()
  "Return the full Ediff help message in Chinese.
Suitable for use as `ediff-long-help-message-function'."
  (concat ediff-long-help-message-head-cn
          (cond (ediff-word-mode
                 ediff-long-help-message-word-mode-cn)
                (ediff-narrow-job
                 ediff-long-help-message-narrow2-cn)
                (ediff-merge-job
                 ediff-long-help-message-merge-cn)
                (ediff-diff3-job
                 ediff-long-help-message-compare3-cn)
                (t
                 ediff-long-help-message-compare2-cn))
          ediff-long-help-message-tail-cn))

;;;###autoload
(defun ediff-brief-help-message-cn ()
  "Return the brief Ediff help message in Chinese.
Suitable for use as `ediff-brief-help-message-function'."
  ediff-brief-message-string-cn)

;;;###autoload
(defun ediff-enable-chinese-help ()
  "Enable Chinese help messages for Ediff.
This adds `ediff--set-chinese-help-in-control-buffer' to
`ediff-mode-hook', so that every new Ediff control buffer uses
Chinese help messages."
  (interactive)
  (add-hook 'ediff-mode-hook #'ediff--set-chinese-help-in-control-buffer)
  (message "Ediff 中文帮助已启用（将在新的 Ediff 会话中生效）"))

(defun ediff-disable-chinese-help ()
  "Restore the default English help messages for Ediff.
This removes `ediff--set-chinese-help-in-control-buffer' from
`ediff-mode-hook'."
  (interactive)
  (remove-hook 'ediff-mode-hook #'ediff--set-chinese-help-in-control-buffer)
  (message "Ediff English help restored"))

(defun ediff--set-chinese-help-in-control-buffer ()
  "Set Chinese help message functions in the Ediff control buffer.
This function is meant to be added to `ediff-mode-hook'."
  (setq ediff-long-help-message-function #'ediff-long-help-message-cn)
  (setq ediff-brief-help-message-function #'ediff-brief-help-message-cn))

(provide 'ediff-help-cn)
;;; ediff-help-cn.el ends here
