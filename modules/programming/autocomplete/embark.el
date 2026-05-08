;;; -*- lexical-binding: t; -*-

(defun my/embark-which-key-indicator ()
  "Use Which-Key to show Embark actions."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s" (plist-get (car targets) :type)))
       keymap
       nil nil t
       (when prefix
         (pcase (lookup-key keymap prefix 'accept-default)
           ((and (pred keymapp) km) km)
           (_ nil)))))))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("M-o" . embark-act))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators
   '(my/embark-which-key-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :config
  ;; Avoid accidentally rebinding keys through Embark actions.
  (define-key embark-command-map (kbd "g") nil)
  (define-key embark-command-map (kbd "l") nil)

  ;; Make collect buffers pleasant with Meow/motion-style navigation too.
  (define-key embark-collect-mode-map (kbd "j") #'next-line)
  (define-key embark-collect-mode-map (kbd "k") #'previous-line))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
