;;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :straight t
  :hook (lsp-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; A large collection of ready-made snippets for many languages
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)
