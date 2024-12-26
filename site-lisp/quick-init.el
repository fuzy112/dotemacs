;; -*- lexical-binding: t; coding: utf-8-unix; -*-

(require-theme 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t)
(modus-themes-load-vivendi)

(setq completion-styles '(flex basic))
(setq tab-always-indent 'complete)

(setq version-control t
      kept-new-versions 9
      kept-old-versions 9
      delete-old-versions t)

(recentf-mode)
(save-place-mode)
(let ((inhibit-message t))
  (repeat-mode))
(electric-pair-mode)

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-jump)
(keymap-global-set "M-o" #'other-window)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; indent-tabs-mode: t
;; End:
