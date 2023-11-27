;;; -*- lexical-binding: t; -*-
;; Vertico is used for autocompletion in the minibuffer
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ("C-l" . vertico-directory-delete-word)
         ("M-g" . vertico-multiform-grid)
         ("M-q" . vertico-multiform-flat))
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))

            ;; Needed with `read-file-name-completion-ignore-case'.
            ;; See these links:
            ;; - https://github.com/minad/vertico/issues/341
            ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=60264
            ;;
            ;; Regardless of it fixing an actual bug, I prefer
            ;; this behavior.
            (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))