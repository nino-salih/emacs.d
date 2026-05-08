;;; -*- lexical-binding: t; -*-

(use-package vterm
  :straight t
  :commands (vterm vterm-other-window)
  :bind (("C-c t" . vterm)
         ("C-c T" . vterm-other-window))
  :custom
  (vterm-always-compile-module t)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  :config
  (setq vterm-shell (or (getenv "SHELL") "/bin/fish")))

