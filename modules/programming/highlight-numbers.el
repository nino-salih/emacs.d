;;; -*- lexical-binding: t; -*-

(defface my/number-literal-face
  nil
  "Face for numeric literals highlighted via tree-sitter.")

(defun my/number-literal-apply-face (&rest _)
  "Set `my/number-literal-face' using the active Doom theme color."
  (set-face-attribute 'my/number-literal-face nil
                      :foreground (or (and (fboundp 'doom-color)
                                           (doom-color 'numbers))
                                      "#d19a66")
                      :weight 'bold))

(defconst my/ts-number-node-re
  (concat "^"
          (regexp-opt
           '("num" "number" "numeric"
             "int" "integer"
             "float" "double"
             "decimal" "hex" "hexadecimal"
             "octal" "binary"
             "long" "short" "byte"
             "imaginary" "rational" "complex"))
          "\\(?:$\\|_\\)")
  "Regexp for tree-sitter node types that are numeric literals.
Matches the word exactly at the start, followed by end-of-string or '_',
so e.g. num_lit, integer_literal, number (JS), int_literal (Go) match
but interface_declaration, function_item, string_literal do not.")

(defun my/ts-number-matcher (limit)
  "Font-lock matcher for numeric literals via tree-sitter up to LIMIT."
  (when (treesit-parser-list)
    (catch 'found
      (while (re-search-forward "[0-9]" limit t)
        (let* ((pos  (1- (point)))
               (node (treesit-node-at pos))
               (type (and node (treesit-node-type node)))
               (nend (and node (treesit-node-end node))))
          (when (and type
                     (string-match-p my/ts-number-node-re type)
                     (<= nend limit))
            (set-match-data (list (treesit-node-start node) nend))
            (goto-char nend)
            (throw 'found t))))
      nil)))

(defun my/number-highlight-setup ()
  "Add tree-sitter number highlighting for the current prog-mode buffer."
  (font-lock-add-keywords nil
    '((my/ts-number-matcher (0 'my/number-literal-face t)))
    'append))

(add-hook 'prog-mode-hook #'my/number-highlight-setup)

(with-eval-after-load 'doom-themes
  (my/number-literal-apply-face)
  (add-hook 'enable-theme-functions #'my/number-literal-apply-face))
