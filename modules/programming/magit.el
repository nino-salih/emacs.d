;;; -*- lexical-binding: t; -*-

(use-package magit
  :straight t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-status)
         ("C-c M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers 'dontask)
  (magit-repository-directories '(("~/Projects" . 2)
                                  ("~/projects" . 2)
                                  ("~/.emacs.d" . 0))))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-x g" "magit status"
    "C-c g" "magit status"
    "C-c M-g" "magit dispatch"))
