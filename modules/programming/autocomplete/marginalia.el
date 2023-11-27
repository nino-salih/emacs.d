;;; -*- lexical-binding: t; -*-
;; Marginalia is a library that provides a convenient interface to marginalize
;; and group the completions returned by various completion backends.
(use-package marginalia
  :straight t
  :after vertico
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once vertico is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))