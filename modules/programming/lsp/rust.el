;;; -*- lexical-binding: t; -*-

(defvar rust-required-components '("rust-analyzer" "clippy" "rustfmt")
  "rustup components to ensure are installed when opening a Rust file.")

(defun rust-ensure-toolchain ()
  "Ensure required rustup components are installed.
Errors if rustup is not found. Installs any missing components
asynchronously so it does not block Emacs."
  (if (not (executable-find "rustup"))
      (display-warning 'rust
                       "rustup not found on PATH — cannot auto-install Rust components."
                       :error)
    (dolist (component rust-required-components)
      (let ((buf-name (format " *rustup-install-%s*" component)))
        (unless (zerop (call-process "rustup" nil nil nil
                                     "component" "list" "--installed"
                                     "--toolchain" "stable"))
          ;; rustup component list failed; skip silently.
          nil)
        (with-temp-buffer
          (call-process "rustup" nil t nil
                        "component" "list" "--installed")
          (unless (search-backward component nil t)
            (message "Rust: installing %s via rustup..." component)
            (set-process-sentinel
             (start-process (format "rustup-install-%s" component)
                            (get-buffer-create buf-name)
                            "rustup" "component" "add" component)
             (lambda (proc event)
               (if (string-prefix-p "finished" event)
                   (message "Rust: %s installed successfully." component)
                 (display-warning 'rust
                                  (format "rustup failed to install %s: %s"
                                          component event)
                                  :warning))))))))))

(defalias 'my/lsp-rust-analyzer-expand-macro-and-focus
  (my/command-and-focus-buffer
   #'lsp-rust-analyzer-expand-macro
   (lambda ()
     (format "*rust-analyzer macro expansion %s*"
             (lsp-workspace-root default-directory))))
  "Expand the Rust macro at point and focus the expansion buffer.")

(defalias 'my/lsp-fix-or-refactor
  #'lsp-execute-code-action
  "Apply an available LSP fix or refactoring at point.")

(defalias 'my/lsp-rust-analyzer-syntax-tree-and-focus
  (my/command-and-focus-buffer
   #'lsp-rust-analyzer-syntax-tree
   (lambda ()
     (format "*rust-analyzer syntax tree %s*"
             (lsp-workspace-root default-directory))))
  "Display the Rust syntax tree and focus its buffer.")

(defalias 'my/lsp-rust-analyzer-status-and-focus
  (my/command-and-focus-buffer
   #'lsp-rust-analyzer-status
   (lambda ()
     (format "*rust-analyzer status %s*"
             (lsp-workspace-root default-directory))))
  "Display rust-analyzer status and focus its buffer.")

(defalias 'my/lsp-rust-analyzer-view-item-tree-and-focus
  (my/command-and-focus-buffer
   #'lsp-rust-analyzer-view-item-tree
   "*rust-analyzer item tree*")
  "Display the Rust item tree and focus its buffer.")

(defalias 'my/lsp-rust-analyzer-view-hir-and-focus
  (my/command-and-focus-buffer
   #'lsp-rust-analyzer-view-hir
   "*rust-analyzer hir*")
  "Display Rust HIR and focus its buffer.")

(defface rust-format-brace-face
  '((t :inherit font-lock-builtin-face))
  "Face for the { } braces of Rust format placeholders.")

(defface rust-format-identifier-face
  '((t :inherit font-lock-variable-name-face))
  "Face for the identifier/spec inside Rust format placeholders like {name} or {:?}.")

(defun rust--format-placeholder-matcher (limit)
  "Match Rust format placeholders {…} inside string literals up to LIMIT.
Jumps directly to string regions instead of scanning every { in code."
  (let (result)
    (while (and (not result) (< (point) limit))
      (if (nth 3 (syntax-ppss))
          ;; Inside a string: search for a placeholder
          (if (re-search-forward "\\({\\)\\([^{}\n]*\\)\\(}\\)" limit t)
              (setq result t)
            (goto-char limit))          ; no placeholder in this string → done
        ;; Outside a string: jump to the next opening quote
        (unless (re-search-forward "\"" limit t)
          (goto-char limit))))          ; no more strings → done
    result))

(use-package rust-mode
  :straight t
  :ensure-system-package rustup
  :init 
  (setq rust-mode-treesiter-derive t)
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map
         ("C-c f" . my/lsp-fix-or-refactor)
         ("C-c e" . my/lsp-rust-analyzer-expand-macro-and-focus)
         ([remap lsp-rust-analyzer-expand-macro] . my/lsp-rust-analyzer-expand-macro-and-focus)
         ([remap lsp-rust-analyzer-syntax-tree] . my/lsp-rust-analyzer-syntax-tree-and-focus)
         ([remap lsp-rust-analyzer-status] . my/lsp-rust-analyzer-status-and-focus)
         ([remap lsp-rust-analyzer-view-item-tree] . my/lsp-rust-analyzer-view-item-tree-and-focus)
         ([remap lsp-rust-analyzer-view-hir] . my/lsp-rust-analyzer-view-hir-and-focus))
  :config
  (font-lock-add-keywords 'rust-mode
    '((rust--format-placeholder-matcher
       (1 'rust-format-brace-face t)
       (2 'rust-format-identifier-face t t)  ; laxmatch: group 2 is empty for {}
       (3 'rust-format-brace-face t)))
    'append)
  (with-eval-after-load 'which-key
    (which-key-add-major-mode-key-based-replacements 'rust-mode
      "C-c f" "fix/refactor"
      "C-c e" "expand macro"))
  :hook
  (rust-mode . rust-ensure-toolchain)
  (rust-mode . lsp))

(with-eval-after-load 'lsp-rust
  ;; Auto-download rust-analyzer if not found on PATH.
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  ;; Inlay hints: show types, parameter names, chained method types.
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-closure-return-type-hints "always")
  ;; Proc macros and build scripts for full analysis.
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-cargo-run-build-scripts t)
  ;; Use Clippy diagnostics in rust-analyzer, so its suggestions become
  ;; available through `lsp-execute-code-action' where rust-analyzer supports it.
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; Show the full signature block in eldoc (everything before the --- separator),
  ;; not just the first line.
  (cl-defmethod lsp-clients-extract-signature-on-hover
    (contents (_server-id (eql rust-analyzer)))
    "Return the signature section from rust-analyzer hover CONTENTS."
    (let* ((rendered  (s-trim (lsp--render-element contents)))
           (lines     (s-lines rendered))
           (sep-re    "\\`[[:space:]]*[-─━═—–]+[[:space:]]*\\'")
           (sections  (let (current acc)
                        (dolist (line lines)
                          (if (string-match-p sep-re line)
                              (progn (when current
                                       (push (nreverse current) acc))
                                     (setq current nil))
                            (push line current)))
                        (when current (push (nreverse current) acc))
                        (nreverse acc)))
           (keep      (if (> (length sections) 1) (butlast sections) sections))
           (non-comment (seq-filter
                         (lambda (line) (not (string-prefix-p "//" line)))
                         (apply #'append keep)))
           (result    (string-trim (string-join non-comment "\n"))))
      (if (string-empty-p result) (car lines) result))))
