;;; -*- lexical-binding: t; -*-

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(defmacro my/keymap! (name &rest args)
  "Define keymap NAME with bindings and optional which-key labels.

Each binding: KEY COMMAND [:wk LABEL]
Optional :doc DOC before the bindings.

Example:
  (my/keymap! my/win-map
    :doc \"Window commands.\"
    \"w\" #\='ace-window    :wk \"ace window\"
    \"d\" #\='delete-window)"
  (let (doc bindings)
    (when (eq (car args) :doc)
      (setq doc (cadr args) args (cddr args)))
    (let ((rest args))
      (while rest
        (let* ((key   (pop rest))
               (cmd   (pop rest))
               (label (when (eq (car rest) :wk)
                        (pop rest)          ; discard :wk
                        (pop rest))))
          (push (list key cmd label) bindings))))
    (setq bindings (nreverse bindings))
    (let (km-body wk-body)
      (dolist (b bindings)
        (let* ((key  (car b))
               (cmd  (cadr b))
               (lbl  (caddr b))
               (sym  (if (and (consp cmd) (eq (car cmd) 'function))
                         (cadr cmd)
                       cmd))
               (desc (or lbl (symbol-name sym))))
          (setq km-body (append km-body (list key cmd)))
          (setq wk-body (append wk-body (list key `'(,desc . ,sym))))))
      `(progn
         (defvar-keymap ,name
           ,@(when doc (list :doc doc))
           ,@km-body)
         (with-eval-after-load 'which-key
           (which-key-add-keymap-based-replacements ,name
             ,@wk-body))))))

(defun my/ace-window-or-other-window ()
  "Use `other-window' for two windows, otherwise use `ace-window'."
  (interactive)
  (if (= (count-windows) 2)
      (other-window 1)
    (ace-window 1)))

(my/keymap! my/window-prefix-map
  :doc "Window commands."
  "w" #'my/ace-window-or-other-window :wk "ace/other window"
  "W" #'other-window                  :wk "other window"
  "d" #'delete-window                 :wk "delete window"
  "D" #'delete-other-windows          :wk "delete other windows"
  "s" #'split-window-below            :wk "split below"
  "f" #'split-window-right            :wk "split right")

(keymap-set global-map "C-c w" (cons "windows" my/window-prefix-map))
(keymap-set global-map "C-c p" #'meow-paren-mode)

(my/keymap! my/meow-advanced-motion-mode-map
  :doc "Advanced motion commands."
  "C-c m" #'meow-advanced-motion-mode :wk "advanced motion state")

(define-minor-mode my/meow-advanced-motion-bindings-mode
  "Enable Advanced motion bindings."
  :init-value nil
  :keymap my/meow-advanced-motion-mode-map)

(defun my/sync-meow-advanced-motion-bindings ()
  "Enable or disable advanced motion bindings based on meow-motion-mode."
  (if meow-motion-mode
      (my/meow-advanced-motion-bindings-mode 1)
    (my/meow-advanced-motion-bindings-mode -1)))

(add-hook 'meow-motion-mode-hook #'my/sync-meow-advanced-motion-bindings)

(my/keymap! my/lsp-help-prefix-map
  :doc "LSP help commands."
  "b" #'help-go-back          :wk "back"
  "c" #'help-customize        :wk "customize"
  "f" #'help-go-forward       :wk "forward"
  "g" #'revert-buffer         :wk "refresh"
  "h" #'describe-mode         :wk "describe mode"
  "i" #'help-goto-info        :wk "info"
  "l" #'help-goto-lispref-info :wk "lispref info"
  "m" #'push-button           :wk "push button"
  "n" #'help-goto-next-page   :wk "next page"
  "p" #'help-goto-previous-page :wk "previous page"
  "q" #'quit-window           :wk "quit window"
  "s" #'help-view-source      :wk "view source")

(defun my/lsp-help-use-meow-bindings ()
  "Prefer Meow bindings in `lsp-help-mode' buffers."
  (meow-motion-mode))

(with-eval-after-load 'meow
  (add-hook 'lsp-help-mode-hook #'my/lsp-help-use-meow-bindings))

(with-eval-after-load 'lsp-mode
  (keymap-set lsp-help-mode-map "TAB" #'forward-button)
  (keymap-set lsp-help-mode-map "<backtab>" #'backward-button)
  (keymap-set lsp-help-mode-map "RET" #'push-button)
  (keymap-set lsp-help-mode-map "C-c h" (cons "help" my/lsp-help-prefix-map)))

(use-package ace-window
  :straight t
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(with-eval-after-load 'flycheck
  (keymap-set flycheck-mode-map "C-c !" (cons "flycheck" flycheck-command-map)))

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "K" #'dired-up-directory)
  (keymap-set dired-mode-map "J" #'dired-find-file))
