;; This Package is used to show linting errors and co
(defun my/flycheck-buffer-after-save ()
  "Run Flycheck for the current buffer after saving."
  (when flycheck-mode
    (flycheck-buffer)))

(defun my/flycheck-enable-for-programming ()
  "Enable Flycheck and re-check the buffer after each save."
  (flycheck-mode 1)
  (add-hook 'after-save-hook #'my/flycheck-buffer-after-save nil t))

(use-package flycheck
  :straight t
  :hook (prog-mode . my/flycheck-enable-for-programming)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))
