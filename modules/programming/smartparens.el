;;; -*- lexical-binding: t; -*-

(use-package smartparens
  :straight t
  :hook
  ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode
    clojure-mode clojurescript-mode clojurec-mode
    cider-repl-mode)
   . smartparens-strict-mode)
  ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (with-eval-after-load 'clojure-mode
    (require 'smartparens-clojure)))
