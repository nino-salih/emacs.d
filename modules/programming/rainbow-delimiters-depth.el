;;; -*- lexical-binding: t; -*-
;;; rainbow-delimiters-depth.el --- Tree-sitter query-based rainbow depth

;; Strategy:
;;  - Use compiled treesit queries to capture anonymous delimiter nodes
;;    ("(", ")", "{", "}", "[", "]") in one shot.
;;  - Walk the tree upward to count per-type nesting depth.
;;  - When two different closing delimiters share the same face slot,
;;    bump the inner one so they differ visually.
;;
;; No manual child-scanning loops — queries handle node-finding.

;;; Compiled query cache ---------------------------------------------------

(defvar my/rts--query-cache (make-hash-table :test #'eq)
  "Cache of compiled treesit queries, keyed by language symbol.")

(defconst my/rts--query-pattern
  ;; Capture every anonymous delimiter by type.
  ;; Using string form because it is compact and compiled later.
  "[ \"(\" \")\" \"{\" \"}\" \"[\" \"]\" \"<\" \">\" ] @delimiter"
  "Tree-sitter query pattern that captures all delimiter nodes.")

(defun my/rts--query (language)
  "Return a compiled treesit query for LANGUAGE (symbol), lazily cached."
  (or (gethash language my/rts--query-cache)
      (when (treesit-language-available-p language)
        (condition-case _
            (puthash language
                     (treesit-query-compile language my/rts--query-pattern)
                     my/rts--query-cache)
          (treesit-query-error nil)))))

;;; Parser bootstrap -------------------------------------------------------

(defun my/rts--ensure-parser ()
  "Create a treesit parser for the current buffer when none exists.
Derives the language from `major-mode'."
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (not (treesit-parser-list)))
    (let* ((base (replace-regexp-in-string "-ts-mode\\|-mode" ""
                                           (symbol-name major-mode)))
           (lang (intern base)))
      (when (treesit-language-available-p lang)
        (treesit-parser-create lang)))))

;;; Core depth logic -------------------------------------------------------

;; Delimiter pairs: open char -> close char (all as strings matching node type)
(defconst my/rts--pairs
  '(("(" . ")") ("{" . "}") ("[" . "]") ("<" . ">"))
  "Alist of open->close delimiter type strings.")

(defun my/rts--pair-for (type)
  "Return (open . close) for node TYPE string, or nil."
  (or (assoc  type my/rts--pairs)           ; TYPE is the open
      (rassoc type my/rts--pairs)))         ; TYPE is the close

(defun my/rts--closingp (type)
  "Non-nil when TYPE is a closing delimiter type."
  (rassoc type my/rts--pairs))

(defun my/rts--face-slot (depth)
  "Map 1-based DEPTH to a rainbow face slot index."
  (1+ (mod (1- depth) rainbow-delimiters-max-face-count)))

(defun my/rts--container-for (node open-type)
  "Return the smallest ancestor of NODE whose first anonymous child is OPEN-TYPE.
This is the container that directly wraps NODE."
  (let ((cur (treesit-node-parent node))
        found)
    (while (and cur (not found))
      (let ((fc (treesit-node-child cur 0)))
        (when (and fc (equal (treesit-node-type fc) open-type))
          (setq found cur)))
      (setq cur (treesit-node-parent cur)))
    found))

(defun my/rts--type-depth (node open-type)
  "Count how many OPEN-TYPE bracket levels enclose NODE.
Returns a 1-based integer — 1 means outermost visible level."
  (let ((depth 0)
        (cur node))
    (while-let ((container (my/rts--container-for cur open-type)))
      (setq depth (1+ depth))
      (setq cur container))
    (max 1 depth)))

(defun my/rts--bracket-depth (loc)
  "Return the per-bracket-type depth for the delimiter at LOC.
Returns nil when treesit is unavailable or LOC is not a delimiter."
  (when-let* ((parser (car (treesit-parser-list)))
              (lang   (treesit-parser-language parser))
              (query  (my/rts--query lang))
              (node   (treesit-node-at loc))
              (type   (treesit-node-type node))
              (pair   (my/rts--pair-for type)))
    (let* ((open-type (car pair))
           (closingp  (my/rts--closingp type))
           (depth     (my/rts--type-depth node open-type)))
      ;; Collision check: when two different closing delimiters are directly
      ;; adjacent and would get the same face slot, bump this one.
      (when closingp
        (let ((prev-pos (and (> (treesit-node-start node) (point-min))
                             (1- (treesit-node-start node)))))
          (when prev-pos
            (let* ((prev-node (treesit-node-at prev-pos))
                   (prev-type (and prev-node (treesit-node-type prev-node)))
                   (prev-pair (and prev-type (my/rts--pair-for prev-type))))
              (when (and prev-pair
                         (my/rts--closingp prev-type)
                         (not (equal open-type (car prev-pair))))
                (let ((prev-depth (my/rts--type-depth prev-node (car prev-pair))))
                  (while (= (my/rts--face-slot depth)
                            (my/rts--face-slot prev-depth))
                    (setq depth (1+ depth)))))))))
      depth)))

;;; rainbow-delimiters integration -----------------------------------------

(defun my/rainbow-pick-face-per-type (depth match loc)
  "Select a rainbow face using per-bracket-type treesit depth.
Falls back to DEPTH when treesit data is unavailable."
  (if (or (<= depth 0) (not match))
      (rainbow-delimiters-default-pick-face depth match loc)
    (my/rts--ensure-parser)
    (let ((ts-depth (my/rts--bracket-depth loc)))
      (rainbow-delimiters-default-pick-face
       (or ts-depth depth) match loc))))

(with-eval-after-load 'rainbow-delimiters
  (setq rainbow-delimiters-pick-face-function
        #'my/rainbow-pick-face-per-type))

(provide 'my-rainbow-delimiters-depth)
