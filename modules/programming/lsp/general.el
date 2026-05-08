;;; -*- lexical-binding: t; -*-

(setq lsp-enable-tree-sitter t)

;; Pin grammar versions for languages whose packages manage their own grammar.
;; clojure-ts-mode needs the exact unstable-20250526 tag; treesit-auto must not
;; override this with a newer/different commit.
;;(add-to-list 'treesit-language-source-alist
;;             '(clojure "https://github.com/sogaiu/tree-sitter-clojure.git"
;;                       "unstable-20250526"))

;; Install grammars that are not already installed:
;; (dolist (lang treesit-language-source-alist)
;;   (unless (treesit-language-available-p (car lang))
;;     (treesit-install-language-grammar (car lang))))

;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; treesit-auto is disabled; each language package installs its own grammar.
 (use-package treesit-auto
   :straight t
   :custom
  (treesit-auto-install 't)
   :config
;;   ;; Exclude rust; we use rust-mode instead of rust-ts-mode.
;;   ;; Exclude clojure; clojure-ts-mode manages the grammar at a pinned version.
   (setq treesit-auto-langs (delq 'rust (delq 'clojure treesit-auto-langs)))
   (treesit-auto-add-to-auto-mode-alist 'all)
   (global-treesit-auto-mode))

(defun my/command-and-focus-buffer (command buffer)
  "Return an interactive command that runs COMMAND and focuses BUFFER.
BUFFER can be a buffer name or a function returning a buffer name."
  (lambda ()
    (interactive)
    (let ((buffer-name (if (functionp buffer)
                           (funcall buffer)
                         buffer)))
      (call-interactively command)
      (when-let ((buf (get-buffer buffer-name)))
        (pop-to-buffer buf)))))

(defalias 'my/lsp-describe-and-focus
  (my/command-and-focus-buffer #'lsp-describe-thing-at-point "*lsp-help*")
  "Open LSP documentation and move focus to the help buffer.")

(defun my/lsp-mode-setup-completion ()
  "Configure lsp-mode completion to cooperate with orderless/corfu."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(defun my/lsp-read-key (prompt options)
  "Read one of OPTIONS for PROMPT without opening a minibuffer."
  (let ((key (read-key prompt)))
    (while (not (memq key options))
      (message "Wrong answer")
      (setq key (read-key prompt)))
    key))

(use-package lsp-mode
  :straight t
  :after (corfu treesit-auto)
  :custom
  (lsp-completion-provider :none) ;; We use Corfu.
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-eldoc-enable-hover t)           ; Typ/Signatur in der Echo-Area beim Hover
  (lsp-eldoc-render-all nil)           ; nur Signatur, kein Dokumentationstext
  :bind (:map lsp-mode-map
         ("C-c h" . my/lsp-describe-and-focus)) ; Doku on-demand, Fokus auf Buffer
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (tsx-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (c-ts-mode . lsp)
  (js-ts-mode . lsp)
  (css-ts-mode . lsp)
  (bash-ts-mode . lsp)
  (java-ts-mode . lsp)
  (json-ts-mode . lsp)
  (yaml-ts-mode . lsp)
  :config
  ;; `lsp--find-root-interactively' asks whether to import a project via
  ;; `read-char-from-minibuffer'.  In this setup that minibuffer prompt can
  ;; trip over modal/minibuffer key handling; a direct key read is enough.
  (advice-add #'lsp--read-char :override #'my/lsp-read-key))

;; Optionally if you want to use a debugger.
(use-package dap-mode
  :straight t
  :after lsp-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language.
