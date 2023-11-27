(setq lsp-enable-tree-sitter t)

(setq treesit-language-source-alist
      '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
        (clojure    . ("https://github.com/sogaiu/tree-sitter-clojure"))
        (commonlisp . ("https://github.com/theHamsta/tree-sitter-commonlisp"))
        (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (fish       . ("https://github.com/ram02z/tree-sitter-fish"))
        (gitignore  . ("https://github.com/shunsambongi/tree-sitter-gitignore"))
        (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
        (graphql    . ("https://github.com/bkegley/tree-sitter-graphql"))
        (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
        (latex      . ("https://github.com/latex-lsp/tree-sitter-latex"))
        (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
        (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown"))
        (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (scss       . ("https://github.com/serenadeai/tree-sitter-scss"))
        (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
        (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
        
        ))


;; ;; Install grammers that are not already installed
;; (dolist (lang treesit-language-source-alist)
;;   (unless (treesit-language-available-p (car lang))
;;     (treesit-install-language-grammar (car lang))))


;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 't)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(defun hook-lsp-to-ts-modes ()
  "Hook lsp-mode to all major modes with a suffix of -ts-mode."
  (dolist (mode (apropos-internal ".*-ts-mode$"))
    (when (string-suffix-p "-ts-mode" (symbol-name mode))
      (add-hook (intern (symbol-name mode)) #'lsp-deferred))))


(use-package lsp-mode
    :straight t
    :after (treesit-auto corfu)      
    ;;:hook (typescript-mode . lsp)
    :custom
    (lsp-completion-provider :none) ;; we use Corfu!
    :init
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless
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
    )

(with-eval-after-load 'lsp-mode
  (hook-lsp-to-ts-modes))

;; optionally if you want to use a debugger
(use-package dap-mode
    :straight t
    :after (lsp-mode)
    )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :straight t
    :config
    (which-key-mode))
