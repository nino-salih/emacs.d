;;; -*- lexical-binding: t; -*-
;;  Orderless is used for fuzzy completion.
(use-package orderless
  :straight t
  :after vertico
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  orderless-component-separator #'orderless-escapable-split-on-space)

            ;; Use the built-in "partial-completion" style to complete
            ;; file inputs such as "/e/ni/co.nix" into
            ;; "/etc/nixos/configuration.nix".  The "basic" style is
            ;; needed to support the hostname completion in the TRAMP
            ;; inputs such as "/sshx:HOSTNAME".
            (setq completion-category-defaults nil
                  completion-category-overrides '((file (styles basic partial-completion))))

            ;; Make the stock file completion styles ("basic" and
            ;; "partial-completion") case insensitive, it fits better
            ;; with the behavior provided by orderless.  See the
            ;; `orderless-smart-case' documentation for how it
            ;; interacts with orderless itself (spoiler: in this setup
            ;; it doesn't).
            (setq read-file-name-completion-ignore-case t)

            (setq completion-styles '(orderless basic))

            (defun vifon/call-without-orderless-dispatchers (orig &rest args)
              "Use with `advice-add' (`:around') to ignore the dispatchers."
              (let ((orderless-style-dispatchers nil))
                (apply orig args)))))