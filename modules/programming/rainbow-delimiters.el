;;; -*- lexical-binding: t; -*-

;;(load (expand-file-name "modules/programming/rainbow-delimiters-depth.el"
;;                        user-emacs-directory)
;;      nil t)

(defun my/rainbow-delimiters-color (name fallback)
  "Return Doom theme color NAME or FALLBACK when Doom has no such color."
  (or (and (fboundp 'doom-color) (doom-color name))
      fallback))

(defun my/rainbow-delimiters-set-depth-face (depth color)
  "Set rainbow delimiter DEPTH face to Doom theme COLOR."
  (set-face-attribute (intern (format "rainbow-delimiters-depth-%d-face" depth))
                      nil
                      :foreground (my/rainbow-delimiters-color color nil)
                      :weight 'normal))

(defun my/rainbow-delimiters-apply-doom-faces (&rest _)
  "Use colors from the active Doom theme for `rainbow-delimiters'."
  (set-face-attribute 'rainbow-delimiters-base-face nil
                      :inherit 'default
                      :weight 'normal)
  (my/rainbow-delimiters-set-depth-face 1 'violet)
  (my/rainbow-delimiters-set-depth-face 2 'green)
  (my/rainbow-delimiters-set-depth-face 3 'orange)
  (my/rainbow-delimiters-set-depth-face 4 'yellow)
  (my/rainbow-delimiters-set-depth-face 5 'cyan)
  (my/rainbow-delimiters-set-depth-face 6 'magenta)
  (my/rainbow-delimiters-set-depth-face 7 'teal)
  (my/rainbow-delimiters-set-depth-face 8 'violet)
  (my/rainbow-delimiters-set-depth-face 9 'green)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground (my/rainbow-delimiters-color 'red "red")
                      :background (my/rainbow-delimiters-color 'bg-alt nil)
                      :weight 'bold
                      :inverse-video nil)
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :inherit 'rainbow-delimiters-unmatched-face))

(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (prog-mode . my/rts--ensure-parser))
  :config
  (my/rainbow-delimiters-apply-doom-faces)
  (add-hook 'enable-theme-functions #'my/rainbow-delimiters-apply-doom-faces))
