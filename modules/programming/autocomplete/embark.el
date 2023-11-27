;;; -*- lexical-binding: t; -*-
;; Embark is a library for Emacs that makes it easy to use keybindings
(use-package embark
  :straight t
  :bind (("C-c a o" . embark-dwim)
         ("C-c a ."   . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act)
         :map embark-command-map
         ;; Unbind the dangerous `global-set-key' and `local-set-key'
         ;; actions.  It's far too easy to accidentally bind over some
         ;; `self-insert-command' binding or even over
         ;; \\[keyboard-quit].
         ("g" . nil)
         ("l" . nil)
         :map embark-collect-mode-map
         ("m" . vifon/embark-select-and-forward))
  :config (progn
            (setq embark-mixed-indicator-delay 2)

            ;; Make the eval action editable.  Evaluating code
            ;; in-place is simple enough without Embark, if I invoke
            ;; it with Embark, I almost definitely want to edit the
            ;; expression beforehand.  And even if not, I can
            ;; just confirm.
            (cl-pushnew 'embark--allow-edit
                        (alist-get 'pp-eval-expression embark-target-injection-hooks))

            ;; Reload the project list after using
            ;; C-u `embark-act' with `project-forget-project'.
            (cl-pushnew 'embark--restart
                        (alist-get 'project-forget-project embark-post-action-hooks))

            (defun embark-act-with-eval (expression)
              "Evaluate EXPRESSION and call `embark-act' on the result."
              (interactive "sExpression: ")
              (with-temp-buffer
                (let ((expr-value (eval (read expression))))
                  (insert (if (stringp expr-value)
                              expr-value
                            (format "%S" expr-value))))
                (embark-act)))

            (dolist (keymap (list embark-variable-map embark-expression-map))
              (define-key keymap (kbd "v") #'embark-act-with-eval))

            ;; Source: https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
            (autoload 'gnus-dired-attach "gnus-dired" nil t)
            (defun embark-attach-file (file)
              "Attach FILE to an email message."
              (interactive "fAttach: ")
              (cl-letf (((symbol-function 'y-or-n-p) #'always))
                (gnus-dired-attach (list file))))
            (bind-key "a" #'embark-attach-file embark-file-map)

            (defun vifon/embark-select-and-forward ()
              (interactive)
              (embark-select)
              (forward-button 1))))


;; Add integration with `consult'
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :if (featurep 'consult))