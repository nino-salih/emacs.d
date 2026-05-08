;;; sdcv.el --- StarDict CLI dictionary interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration of sdcv (StarDict Console Version) with Emacs.
;;
;; Features:
;;  - Persistent sdcv process (started once, cleaned up on Emacs exit)
;;  - Word lookup at point or via interactive live completion
;;  - Results in an org-mode buffer with a navigation minor mode (sdcv-mode)
;;  - Multiple dicts: results separated by org headings
;;  - Per-dict font-lock highlighting via sdcv-highlighting-alist
;;  - ASCII → Org-entity replacement (optional, with blacklist)
;;  - Persistent search history saved to disk
;;  - Dict selection via completing-read / completing-read-multiple
;;
;; Usage:
;;   M-x sdcv-lookup-word-at-point   ; look up word under cursor
;;   M-x sdcv-lookup                 ; interactive with live candidates
;;   M-x sdcv-lookup-all-dicts       ; search across all available dicts
;;   M-x sdcv-lookup-select-dicts    ; choose dicts, then search
;;   M-x sdcv-list-dicts             ; show available dicts
;;
;; Example configuration:
;;   (setq sdcv-preferred-dicts
;;         '((:name "Webster's Revised Unabridged Dictionary" :display "Webster")
;;           (:name "English-German Dictionary" :display "EN→DE" :color "blue")))

;;; Code:

(require 'json)
(require 'org)
(require 'shr)
(require 'cl-lib)
(require 'subr-x)
(require 'xml)

;;; ── Customization ────────────────────────────────────────────────────────────

(defgroup sdcv nil
  "StarDict CLI dictionary interface."
  :group 'applications
  :prefix "sdcv-")

(defcustom sdcv-program "sdcv"
  "Name or absolute path of the sdcv executable.
Defaults to \"sdcv\", resolved via PATH."
  :type 'string
  :group 'sdcv)

(defcustom sdcv-data-dir nil
  "Directory containing StarDict dictionaries.
When nil, sdcv uses its default search paths."
  :type '(choice (const :tag "Use sdcv defaults" nil)
                 (directory :tag "Custom data directory"))
  :group 'sdcv)

(defcustom sdcv-only-data-dir nil
  "When non-nil, restrict sdcv to `sdcv-data-dir' only.
Requires `sdcv-data-dir' to be set."
  :type 'boolean
  :group 'sdcv)

(defcustom sdcv-preferred-dicts nil
  "Ordered list of preferred dictionaries.
Each entry is a plist with following keys:
  :name    (string, required) The bookname as shown by `sdcv -l'.
  :display (string, optional) Human-readable label for completion UIs / headers.
  :color   (string, optional) Color for this dict's org heading face.

Example:
  \\='((:name \"Webster's Revised Unabridged Dictionary\" :display \"Webster\")
    (:name \"English-German Dictionary\" :display \"EN→DE\" :color \"steelblue\"))"
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'sdcv)

(defcustom sdcv-preview-chars 80
  "Number of definition characters to show as annotation in completion."
  :type 'integer
  :group 'sdcv)

(defcustom sdcv-auto-org-entities nil
  "When non-nil, automatically apply org-entity replacement on result buffers.
Note: this iterates over all org-entities on every lookup and can be slow
for large dictionary entries.  Bind it to a key in `sdcv-mode-map' instead.
See `sdcv-apply-org-entities' and `sdcv-org-entities-blacklist'."
  :type 'boolean
  :group 'sdcv)

(defcustom sdcv-org-entities-blacklist nil
  "List of org-entity names that should NOT be replaced by `sdcv-apply-org-entities'.
Each entry is a string matching `(nth 0 entity)' from `org-entities'.
Example: \\='(\"pi\" \"mu\") keeps those ASCII strings unreplaced."
  :type '(repeat string)
  :group 'sdcv)

(defcustom sdcv-highlighting-alist nil
  "Alist of per-dictionary font-lock rules.
Format: ((DICT-NAME . ((REGEX . FACE) ...)) ...)
DICT-NAME is matched against the `:name' from `sdcv-preferred-dicts' or the
raw dict name in the JSON response.

Example:
  \\='((\"Webster's Revised Unabridged Dictionary\"
     . ((\"^  \\\\(Etym:\\|Etymology:\\)\" . font-lock-keyword-face)
        (\"\\\\bobs\\\\.\\\\b\"            . font-lock-comment-face))))"
  :type '(alist :key-type string
                :value-type (repeat (cons regexp face)))
  :group 'sdcv)

(defcustom sdcv-global-highlighting nil
  "Fallback list of (REGEX . FACE) pairs applied when no dict-specific rule matches.
See `sdcv-highlighting-alist'."
  :type '(repeat (cons regexp face))
  :group 'sdcv)

(defcustom sdcv-history-file
  (expand-file-name "sdcv-history.el"
                    (if (boundp 'no-littering-var-directory)
                        no-littering-var-directory
                      (expand-file-name "var" user-emacs-directory)))
  "File for persistent search history.  Set to nil to disable persistence."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'sdcv)

(defcustom sdcv-history-max 100
  "Maximum number of entries kept in the search history."
  :type 'integer
  :group 'sdcv)

(defcustom sdcv-use-persistent-process t
  "When non-nil, keep one sdcv process alive between lookups (faster).
When nil, each lookup spawns a fresh process."
  :type 'boolean
  :group 'sdcv)

(defconst sdcv--default-rendering-rules nil
  "Built-in dictionary rendering rules.

The core package stays dictionary-agnostic.  Dictionary-specific rendering
rules belong in user configuration such as `sdcv-config.el'.")

(defcustom sdcv-rendering-rules sdcv--default-rendering-rules
  "Ordered list of dictionary-specific rendering rules.

Each rule is a plist.  Match keys:
  :name      exact dictionary name
  :regexp    regexp matched against the dictionary name
  :keywords  list of substrings; every keyword must occur in the name
  :predicate function called with the dictionary name

Rendering keys:
  :format       one of `html', `xml', `text', or nil for auto-detection
  :renderer     function called as (FN DEFINITION ENTRY RULE)
  :plain        function called as (FN DEFINITION ENTRY RULE)
  :transform    function called as (FN DEFINITION ENTRY RULE)
  :post-process function or list of functions called as
                (FN START END ENTRY RULE)
  :faces        list of (REGEXP . FACE) rules applied to the rendered section

The first matching rule wins.  This makes the format user-extensible without
hard-coding dictionary names in the lookup logic."
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'sdcv)

(defcustom sdcv-completion-min-input 2
  "Minimum number of characters before completion queries sdcv.
Explicit `*' and `?' wildcards bypass this limit."
  :type 'integer
  :group 'sdcv)

(defcustom sdcv-completion-query-function #'sdcv-default-completion-query
  "Function used to turn minibuffer input into an sdcv completion query.
It receives the raw minibuffer input and must return an sdcv search string or
nil.  The default builder keeps explicit `*' and `?' wildcards, uses the first
space-separated token, and expands plain tokens to prefix queries."
  :type 'function
  :group 'sdcv)

(defcustom sdcv-result-sort-function #'sdcv-sort-results-by-dict-order
  "Function used to sort lookup results before display.
The function receives RESULTS and DICTS and must return the ordered result
list.  The default keeps the order of DICTS and falls back to the order of
`sdcv-preferred-dicts'."
  :type 'function
  :group 'sdcv)

(defcustom sdcv-multi-result-style 'all-in-one
  "How to display multiple dictionary results for a single word lookup.

`all-in-one'  All dictionary entries are inserted into one result buffer
              in dict-preference order (default).
`selector'    A grouped minibuffer completion prompt lets the user pick one
              entry.  Results are grouped by dictionary (Vertico shows a
              header per group).  Single-entry results are shown directly
              without a prompt."
  :type '(choice (const :tag "All results in one buffer" all-in-one)
                 (const :tag "Interactive per-dict selector" selector))
  :group 'sdcv)

;;; ── Internal state ───────────────────────────────────────────────────────────

(defvar sdcv--process nil
  "The persistent sdcv process object.")

(defvar sdcv--accumulator ""
  "Buffer accumulating raw output from the sdcv process.")

(defvar sdcv--response-ready nil
  "Non-nil once a complete JSON array has been detected in `sdcv--accumulator'.")

(defvar sdcv--parsed-result nil
  "Parsed JSON result from the last sdcv process query.")

(defvar sdcv--available-dicts nil
  "Cached list of available dictionary name strings.")

(defvar sdcv-history nil
  "Global list of searched words (newest first).")

;;; ── Process management ───────────────────────────────────────────────────────

(defun sdcv--preferred-dict-names ()
  "Return list of :name strings from `sdcv-preferred-dicts'."
  (mapcar (lambda (d) (plist-get d :name)) sdcv-preferred-dicts))

(defun sdcv--base-args ()
  "Return base argument list shared by persistent and direct invocations."
  (let (args)
    (push "--json-output" args)
    (when sdcv-data-dir
      (push "--data-dir" args)
      (push (expand-file-name sdcv-data-dir) args)
      (when sdcv-only-data-dir
        (push "--only-data-dir" args)))
    (nreverse args)))

(defun sdcv--process-args ()
  "Args for starting the persistent sdcv interactive process."
  (let ((args (sdcv--base-args)))
    (dolist (d (sdcv--preferred-dict-names))
      (setq args (append args (list "--use-dict" d))))
    args))

(defun sdcv--json-complete-p (str)
  "Return non-nil when STR contains at least one complete top-level JSON array.
Uses `json-read' in a temp buffer to detect completeness robustly."
  (let ((start (string-match "\\[" str)))
    (when start
      (condition-case nil
          (with-temp-buffer
            (insert (substring str start))
            (goto-char (point-min))
            (let ((json-array-type 'list)
                  (json-object-type 'alist))
              ;; json-read reads exactly one value and stops; errors if incomplete
              (json-read)
              t))
        (error nil)))))

(defun sdcv--parse-accumulated ()
  "Parse the first complete JSON array in `sdcv--accumulator'.
Return a list of alists or nil on failure."
  (let ((start (string-match "\\[" sdcv--accumulator)))
    (when start
      (condition-case err
          (with-temp-buffer
            (insert (substring sdcv--accumulator start))
            (goto-char (point-min))
            (let ((json-array-type 'list)
                  (json-object-type 'alist))
              (json-read)))
        (error
         (message "sdcv: JSON parse error: %s" (error-message-string err))
         nil)))))

(defun sdcv--process-filter (_proc output)
  "Accumulate OUTPUT and set `sdcv--response-ready' when JSON is complete."
  (setq sdcv--accumulator (concat sdcv--accumulator output))
  (when (sdcv--json-complete-p sdcv--accumulator)
    (setq sdcv--parsed-result (sdcv--parse-accumulated))
    (setq sdcv--response-ready t)))

(defun sdcv--ensure-process ()
  "Start the sdcv interactive process if not already running."
  (unless (and sdcv--process (process-live-p sdcv--process))
    (setq sdcv--accumulator ""
          sdcv--response-ready nil
          sdcv--parsed-result nil)
    (let ((args (sdcv--process-args)))
      (setq sdcv--process
            (apply #'start-process "sdcv" nil sdcv-program args)))
    (set-process-filter sdcv--process #'sdcv--process-filter)
    (set-process-query-on-exit-flag sdcv--process nil)
    ;; Drain startup banner/prompt; give sdcv up to 1 s to be ready
    (accept-process-output sdcv--process 1.0 nil t)))

(defun sdcv-stop-process ()
  "Stop the persistent sdcv process."
  (interactive)
  (when (and sdcv--process (process-live-p sdcv--process))
    (process-send-eof sdcv--process)
    (delete-process sdcv--process))
  (setq sdcv--process nil))

(defun sdcv-restart-process ()
  "Restart the persistent sdcv process (picks up new `sdcv-preferred-dicts')."
  (interactive)
  (sdcv-stop-process)
  (setq sdcv--available-dicts nil)
  (message "sdcv: process restarted"))

(add-hook 'kill-emacs-hook #'sdcv-stop-process)

;;; ── Low-level query ──────────────────────────────────────────────────────────

(defun sdcv--query (word)
  "Look up WORD via the persistent sdcv process.
Return a list of result alists or nil on timeout/error."
  (sdcv--ensure-process)
  (setq sdcv--accumulator ""
        sdcv--response-ready nil
        sdcv--parsed-result nil)
  (process-send-string sdcv--process (concat word "\n"))
  (let ((deadline (+ (float-time) 5.0)))
    (while (and (not sdcv--response-ready)
                (< (float-time) deadline))
      ;; sit-for yields to the event loop → Emacs stays responsive
      (sit-for 0.02)))
  (unless sdcv--response-ready
    (message "sdcv: timeout waiting for response for %S" word))
  sdcv--parsed-result)

(defun sdcv--query-args (word &optional dicts exact-search)
  "Return command-line arguments for looking up WORD.
Restrict the lookup to DICTS when non-nil.  When EXACT-SEARCH is non-nil, add
`--exact-search'."
  (append (sdcv--base-args)
          (list "--non-interactive")
          (when exact-search (list "--exact-search"))
          (mapcan (lambda (dict) (list "--use-dict" dict))
                  (or dicts (sdcv--preferred-dict-names)))
          (list "--" word)))

(defun sdcv--parse-json-results (json)
  "Parse JSON into a list of sdcv result alists.
Return nil when JSON is empty or invalid."
  (condition-case nil
      (let ((json-array-type 'list)
            (json-object-type 'alist))
        (json-read-from-string (string-trim json)))
    (error nil)))

(cl-defun sdcv--query-direct (word &key dicts exact-search)
  "Look up WORD synchronously with `call-process'.
Restrict the lookup to DICTS when non-nil.  When EXACT-SEARCH is non-nil, only
exact headword hits are returned."
  (let* ((args (sdcv--query-args word dicts exact-search))
         (output (with-temp-buffer
                   (apply #'call-process sdcv-program nil t nil args)
                   (buffer-string))))
    (sdcv--parse-json-results output)))

(cl-defun sdcv--do-query (word &key dicts exact-search)
  "Query sdcv for WORD with DICTS.
Uses persistent process when `sdcv-use-persistent-process' is t and no
specific DICTS override is given.  Falls back to `sdcv--query-direct' when
the persistent process times out or returns nil."
  (if (and sdcv-use-persistent-process (null dicts) (null exact-search))
      (or (sdcv--query word)
          (sdcv--query-direct word))
    (sdcv--query-direct word :dicts dicts :exact-search exact-search)))

;;; ── Dict management ──────────────────────────────────────────────────────────

(defun sdcv--fetch-available-dicts ()
  "Invoke `sdcv -l' and return a list of dictionary name strings."
  (let* ((args (append (list "--list-dicts")
                       (when sdcv-data-dir
                         (list "--data-dir" (expand-file-name sdcv-data-dir)))
                       (when (and sdcv-data-dir sdcv-only-data-dir)
                         (list "--only-data-dir"))))
         (output (with-temp-buffer
                   (apply #'call-process sdcv-program nil t nil args)
                   (buffer-string))))
    ;; sdcv -l output:
    ;;   Dictionary's name   Word count
    ;;   Some Dict Name      12345
    ;; Skip the header line; strip trailing whitespace + digit word-count.
    (let ((lines (cdr (split-string output "\n" t))))
      (delq nil
            (mapcar (lambda (l)
                      (let ((name (string-trim
                                   (replace-regexp-in-string
                                    "\\s-+[0-9]+\\s-*$" "" l))))
                        (unless (string-empty-p name) name)))
                    lines)))))

(defun sdcv-available-dicts (&optional refresh)
  "Return the list of available dictionaries, fetching if needed.
With prefix arg REFRESH, force re-fetching."
  (interactive "P")
  (when refresh (setq sdcv--available-dicts nil))
  (or sdcv--available-dicts
      (setq sdcv--available-dicts (sdcv--fetch-available-dicts))))

(defun sdcv--dict-plist (name)
  "Return the plist entry for dict NAME from `sdcv-preferred-dicts', or nil."
  (cl-find name sdcv-preferred-dicts
           :key (lambda (p) (plist-get p :name))
           :test #'string=))

(defun sdcv--dict-display-name (name)
  "Return human-readable label for dict NAME."
  (let ((pref (sdcv--dict-plist name)))
    (if pref (or (plist-get pref :display) name) name)))

(defun sdcv-list-dicts ()
  "Display all available StarDict dictionaries in a dedicated buffer."
  (interactive)
  (let ((dicts (sdcv-available-dicts)))
    (with-current-buffer (get-buffer-create "*sdcv-dicts*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (read-only-mode -1)
        (insert "* Available StarDict Dictionaries\n\n")
        (if (null dicts)
            (insert "/No dictionaries found.  Check `sdcv-data-dir'./\n")
          (dolist (d dicts)
            (let ((pref (sdcv--dict-plist d)))
              (insert (format "- %s%s\n"
                              (if pref
                                  (format "*%s* (=%s=)"
                                          (or (plist-get pref :display) d) d)
                                d)
                              (if pref " ★" ""))))))
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer (current-buffer)))))

(defun sdcv-select-dict ()
  "Interactively select one dictionary via completing-read."
  (interactive)
  (let* ((dicts   (sdcv-available-dicts))
         (choices (mapcar (lambda (d) (cons (sdcv--dict-display-name d) d)) dicts))
         (sel     (completing-read "Dictionary: " (mapcar #'car choices) nil t)))
    (cdr (assoc sel choices #'string=))))

(defun sdcv-select-dicts ()
  "Interactively select multiple dictionaries via completing-read-multiple."
  (interactive)
  (let* ((dicts   (sdcv-available-dicts))
         (choices (mapcar (lambda (d) (cons (sdcv--dict-display-name d) d)) dicts))
         (sel     (completing-read-multiple "Dictionaries: "
                                            (mapcar #'car choices) nil t)))
    (mapcar (lambda (s) (cdr (assoc s choices #'string=))) sel)))

;;; ── Dictionary toggle UI ────────────────────────────────────────────────────

(defcustom sdcv-disabled-dicts nil
  "List of dict :name strings to skip during lookup.
Modified interactively via `sdcv-toggle-dicts'.  Persists across sessions when
set via Customize; for ad-hoc use just setq it."
  :type '(repeat string)
  :group 'sdcv)

(defvar sdcv-dict-toggle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     #'sdcv-dict-toggle-at-point)
    (define-key map (kbd "SPC")     #'sdcv-dict-toggle-at-point)
    (define-key map (kbd "C-c C-c") #'sdcv-dict-toggle-apply)
    (define-key map (kbd "q")       #'quit-window)
    map)
  "Keymap for `sdcv-dict-toggle-mode'.")

(define-derived-mode sdcv-dict-toggle-mode special-mode "sdcv-Dicts"
  "Major mode for the sdcv dictionary toggle buffer."
  (setq-local truncate-lines t))

(defconst sdcv--toggle-header-lines 3
  "Number of non-dict header lines in the toggle buffer.")

(defun sdcv--toggle-line-dict ()
  "Return the dict plist under point, or nil if not on a dict line."
  (let ((idx (- (line-number-at-pos) sdcv--toggle-header-lines 1)))
    (when (and (>= idx 0) (< idx (length sdcv-preferred-dicts)))
      (nth idx sdcv-preferred-dicts))))

(defun sdcv-dict-toggle-at-point ()
  "Toggle the enabled/disabled state of the dictionary on the current line."
  (interactive)
  (when-let ((d (sdcv--toggle-line-dict)))
    (let* ((name           (plist-get d :name))
           (inhibit-read-only t))
      (if (member name sdcv-disabled-dicts)
          (setq sdcv-disabled-dicts (delete name sdcv-disabled-dicts))
        (push name sdcv-disabled-dicts))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "\\[.\\]")
          (replace-match (if (member name sdcv-disabled-dicts) "[ ]" "[X]"))))))) 

(defun sdcv-dict-toggle-apply ()
  "Apply the current selection, restart the sdcv process, and close the buffer."
  (interactive)
  (sdcv-restart-process)
  (message "sdcv: dictionary selection applied (%d disabled)" (length sdcv-disabled-dicts))
  (quit-window))

(defun sdcv-toggle-dicts ()
  "Show a buffer to enable/disable individual dictionaries.
RET/SPC toggles, C-c C-c applies and restarts, q quits."
  (interactive)
  (let ((buf (get-buffer-create "*sdcv-dict-toggle*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "W\u00f6rterb\u00fccher aktivieren / deaktivieren\n")
        (insert "  RET/SPC: umschalten   C-c C-c: Anwenden   q: Beenden\n\n")
        (dolist (d sdcv-preferred-dicts)
          (let* ((name       (plist-get d :name))
                 (display    (or (plist-get d :display) name))
                 (color      (plist-get d :color))
                 (enabled    (not (member name sdcv-disabled-dicts)))
                 (line-start (point)))
            (insert (format "[%s] %s\n" (if enabled "X" " ") display))
            (when color
              (put-text-property line-start (1- (point)) 'face
                                 `(:foreground ,color))))))
      (goto-char (point-min))
      (forward-line sdcv--toggle-header-lines)
      (sdcv-dict-toggle-mode))
    (pop-to-buffer buf)))

;; Exclude disabled dicts from query and process startup args.
(define-advice sdcv--preferred-dict-names (:filter-return (names) sdcv-filter-disabled)
  "Remove dicts listed in `sdcv-disabled-dicts' from the lookup list."
  (if sdcv-disabled-dicts
      (seq-remove (lambda (n) (member n sdcv-disabled-dicts)) names)
    names))

;;; ── Search history ───────────────────────────────────────────────────────────

(defun sdcv--history-load ()
  "Load `sdcv-history' from `sdcv-history-file'."
  (when (and sdcv-history-file (file-readable-p sdcv-history-file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents sdcv-history-file)
          (setq sdcv-history (read (current-buffer))))
      (error (setq sdcv-history nil)))))

(defun sdcv--history-save ()
  "Save `sdcv-history' to `sdcv-history-file'."
  (when sdcv-history-file
    (condition-case nil
        (let ((dir (file-name-directory sdcv-history-file)))
          (unless (file-exists-p dir)
            (make-directory dir t))
          (with-temp-buffer
            (insert (prin1-to-string sdcv-history))
            (write-region (point-min) (point-max)
                          sdcv-history-file nil 'quiet)))
      (error nil))))

(defun sdcv--history-push (word)
  "Add WORD to the front of `sdcv-history', trimming to `sdcv-history-max'."
  (setq sdcv-history (delete word sdcv-history))
  (push word sdcv-history)
  (when (> (length sdcv-history) sdcv-history-max)
    (setcdr (nthcdr (1- sdcv-history-max) sdcv-history) nil))
  (sdcv--history-save))

(add-hook 'kill-emacs-hook #'sdcv--history-save)
(sdcv--history-load)

;;; ── Org-entity replacement ───────────────────────────────────────────────────

(defun sdcv-apply-org-entities ()
  "Replace ASCII representations with \\org-entity names in current buffer.
Respects `sdcv-org-entities-blacklist'.  Calls `my/ascii-to-org-entities'
when the blacklist is empty; otherwise applies an inline variant that skips
blacklisted names."
  (interactive)
  (cond
   ;; If blacklist is empty, delegate directly
   ((and (null sdcv-org-entities-blacklist)
         (fboundp 'my/ascii-to-org-entities))
    (my/ascii-to-org-entities))
   ;; With blacklist: apply the logic ourselves
   (t
    (require 'org-entities)
    (let* ((include-user (and (fboundp 'my/ascii-to-org-entities)
                              (boundp 'my/ascii-to-org-entities-include-user)
                              my/ascii-to-org-entities-include-user))
           (base   (seq-filter #'listp org-entities))
           (user   (when (and include-user (boundp 'org-entities-user))
                     (seq-filter #'listp org-entities-user)))
           (all    (append base user))
           (pairs  (mapcar (lambda (e) (cons (nth 4 e) (nth 0 e))) all))
           (valid  (seq-filter
                    (lambda (p)
                      (and (car p)
                           (not (string= (car p) ""))
                           (not (string= (car p) (cdr p)))
                           ;; skip blacklisted entity names
                           (not (member (cdr p) sdcv-org-entities-blacklist))))
                    pairs))
           (sorted (sort valid (lambda (a b) (> (length (car a)) (length (car b)))))))
      ;; Use case-sensitive search so "ue"→\uuml and "Ue"→\Uuml are distinct.
      (let ((case-fold-search nil))
        (dolist (pair sorted)
          (let* ((ascii       (car pair))
                 (name        (cdr pair))
                 (pattern     (concat "\\b" (regexp-quote ascii) "\\b"))
                 (replacement (concat "\\" name)))
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward pattern nil t)
                (replace-match replacement t t))))))))))

;;; ── Rendering helpers ──────────────────────────────────────────────────────

(defconst sdcv--definition-renderers
  '((html . sdcv--html-insert)
    (xml  . sdcv--xml-insert)
    (text . sdcv--text-insert))
  "Default renderers keyed by definition format.")

(defconst sdcv--definition-plain-renderers
  '((html . sdcv--html-strip)
    (xml  . sdcv--xml-strip)
    (text . sdcv--text-plain))
  "Default plain-text renderers keyed by definition format.")

(defun sdcv--show-buffer-start (buffer)
  "Move point to the beginning of BUFFER in all visible windows."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-min)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (set-window-point window (with-current-buffer buffer (point-min)))
      (set-window-start window (with-current-buffer buffer (point-min)) t))))

(defun sdcv--cleanup-rendered-buffer ()
  "Normalize blank lines and trailing whitespace in the current buffer.
Return the cleaned buffer contents."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n+" nil t)
    (replace-match "\n\n"))
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match ""))
  (string-trim (buffer-string)))

(defun sdcv--org-pre-boundary-p ()
  "Return non-nil when point is at a safe Org emphasis boundary."
  (or (bolp)
      (memq (char-before) '(?\  ?\n ?\t ?\( ?\" ?\' ?\[ ?{))))

(defun sdcv--insert-org-wrapper (delimiter text)
  "Insert TEXT wrapped in DELIMITER with safe Org emphasis boundaries."
  (unless (string-empty-p text)
    (unless (sdcv--org-pre-boundary-p)
      (insert " "))
    (insert delimiter text delimiter)))

(defun sdcv-sort-results-by-dict-order (results dicts)
  "Sort RESULTS according to DICTS or `sdcv-preferred-dicts'."
  (let* ((order (or dicts (sdcv--preferred-dict-names)))
         (ranks (cl-loop for dict in order
                         for index from 0
                         collect (cons dict index))))
    (cl-stable-sort
     (copy-sequence results)
     #'<
     :key (lambda (entry)
            (or (cdr (assoc (alist-get 'dict entry) ranks #'string=))
                most-positive-fixnum)))))

(defun sdcv--sort-results (results &optional dicts)
  "Return RESULTS sorted according to `sdcv-result-sort-function'."
  (if (and sdcv-result-sort-function results)
      (funcall sdcv-result-sort-function results dicts)
    results))

(defun sdcv--clear-style-overlays ()
  "Remove sdcv styling overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'sdcv-overlay t))

(defun sdcv--apply-heading-overlay (start end color)
  "Apply a colored heading overlay between START and END."
  (when color
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'sdcv-overlay t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'priority 1000)
      (overlay-put overlay 'face `(:inherit org-level-1
                                 :foreground ,color
                                 :weight bold)))))

(defun sdcv--apply-face-overlay (start end face &optional priority)
  "Apply FACE as an overlay between START and END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'sdcv-overlay t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'priority (or priority 900))
    (overlay-put overlay 'face face)))

(defun sdcv--apply-shr-face-overlays (start end _entry _rule)
  "Convert shr bold/italic text properties in START..END to face overlays.
Overlays with high priority survive org-mode's font-lock refontification,
whereas text properties set by `shr-insert-document' do not."
  (let ((pos start))
    (while (< pos end)
      (let* ((face  (get-text-property pos 'face))
             (faces (cond ((null face) nil)
                          ((listp face) face)
                          (t (list face))))
             (next  (or (next-single-property-change pos 'face nil end) end))
             bold italic)
        (dolist (f faces)
          (cond
           ((memq f '(bold shr-bold)) (setq bold t))
           ((memq f '(italic shr-italic)) (setq italic t))
           ((and (listp f) (not (null f)))
            (when (memq (plist-get f :weight) '(bold extra-bold semi-bold))
              (setq bold t))
            (when (memq (plist-get f :slant) '(italic oblique))
              (setq italic t)))))
        (when bold
          (sdcv--apply-face-overlay pos next '(:weight bold) 920))
        (when italic
          (sdcv--apply-face-overlay pos next '(:slant italic) 919)))
      (setq pos (or (next-single-property-change pos 'face nil end) end)))))

(defun sdcv--rendering-rule-matches-p (rule dict)
  "Return non-nil when RULE applies to dictionary name DICT."
  (and (or (plist-member rule :name)
           (plist-member rule :regexp)
           (plist-member rule :keywords)
           (plist-member rule :predicate))
       (or (null (plist-get rule :name))
           (string= dict (plist-get rule :name)))
       (or (null (plist-get rule :regexp))
           (string-match-p (plist-get rule :regexp) dict))
       (or (null (plist-get rule :keywords))
           (seq-every-p (lambda (keyword)
                          (string-match-p (regexp-quote keyword) dict))
                        (plist-get rule :keywords)))
       (or (null (plist-get rule :predicate))
           (funcall (plist-get rule :predicate) dict))))

(defun sdcv--rendering-rule (dict)
  "Return the first configured rendering rule for DICT, or an empty plist."
  (or (seq-find (lambda (rule) (sdcv--rendering-rule-matches-p rule dict))
                sdcv-rendering-rules)
      '()))

(defun sdcv--guess-definition-format (definition)
  "Guess the markup format used by DEFINITION."
  (let ((case-fold-search t))
    (cond
     ((not (stringp definition)) 'text)
     ((string-match-p "\\`[[:space:][:cntrl:]]*<\\?xml\\b" definition) 'xml)
     ((string-match-p "\\`[[:space:][:cntrl:]]*<[[:alpha:]][^>]*>" definition)
      (if (string-match-p
           "\\`[[:space:][:cntrl:]]*<\\(?:html\\|body\\|div\\|font\\|span\\|br\\|table\\|tr\\|td\\|p\\|a\\|img\\)\\b"
           definition)
          'html
        'xml))
     (t 'text))))

(defun sdcv--definition-format (definition rule)
  "Return the format symbol for DEFINITION according to RULE."
  (or (plist-get rule :format)
      (sdcv--guess-definition-format definition)))

(defun sdcv--transform-definition (definition entry rule)
  "Apply RULE's transform to DEFINITION for ENTRY, if present."
  (if-let ((transform (plist-get rule :transform)))
      (funcall transform definition entry rule)
    definition))

(defun sdcv--text-plain (text &optional _entry _rule)
  "Return TEXT as plain trimmed text."
  (string-trim (or text "")))

(defun sdcv--text-insert (text &optional _entry _rule)
  "Insert TEXT as a plain definition block."
  (insert (sdcv--text-plain text) "\n"))

(defun sdcv--parse-xml-document ()
  "Parse the current buffer as XML and return its DOM, or nil on failure."
  (condition-case nil
      (if (fboundp 'libxml-parse-xml-region)
          (libxml-parse-xml-region (point-min) (point-max))
        (car (xml-parse-region (point-min) (point-max))))
    (error nil)))

(defun sdcv--xml-walk (node)
  "Insert a readable text representation of XML DOM NODE at point."
  (cond
   ((stringp node) (insert node))
   ((consp node)
    (let ((tag (car node))
          (kids (cddr node)))
      (pcase tag
        ((or 'br 'lb) (insert "\n"))
        ((or 'li 'item)
         (insert "- ")
         (mapc #'sdcv--xml-walk kids)
         (unless (bolp) (insert "\n")))
        ((or 'p 'para 'paragraph 'div 'section 'entry 'sense 'def 'definition
             'gloss 'table 'tr 'row)
         (mapc #'sdcv--xml-walk kids)
         (unless (bolp) (insert "\n")))
        (_ (mapc #'sdcv--xml-walk kids)))))))

(defun sdcv--xml-rendered-string (xml)
  "Return XML rendered as readable text."
  (or (with-temp-buffer
        (insert xml)
        (when-let ((dom (sdcv--parse-xml-document)))
          (erase-buffer)
          (if (and (consp dom) (symbolp (car dom)))
              (sdcv--xml-walk dom)
            (mapc #'sdcv--xml-walk dom))
          (sdcv--cleanup-rendered-buffer)))
      (sdcv--text-plain (replace-regexp-in-string "<[^>]+>" " " xml))))

(defun sdcv--xml-strip (xml &optional _entry _rule)
  "Convert XML into plain text for previews and fallbacks."
  (sdcv--xml-rendered-string xml))

(defun sdcv--xml-insert (xml &optional _entry _rule)
  "Insert XML as readable text."
  (insert (sdcv--xml-rendered-string xml) "\n"))

;;; ── HTML rendering ─────────────────────────────────────────────────────────

(defcustom sdcv-shr-use-colors t
  "When non-nil, preserve HTML foreground/background colors in the result buffer.
Requires libxml2 support in Emacs."
  :type 'boolean
  :group 'sdcv)

(defun sdcv--html-strip (html &optional _entry _rule)
  "Convert HTML to a plain string (no text properties).  Used for annotations."
  (if (and (fboundp 'libxml-parse-html-region) (fboundp 'shr-insert-document))
      (with-temp-buffer
        (insert html)
        (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
               (shr-width          nil)
               (shr-use-fonts      nil)
               (shr-use-colors     nil)
               (shr-inhibit-images t)
               (shr-bullet         "- "))
          (erase-buffer)
          (shr-insert-document dom)
          (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    ;; Fallback: regex tag stripping
    (let* ((s (replace-regexp-in-string "<[Bb][Rr][[:space:]]*/?>", "\n" html))
           (s (replace-regexp-in-string "</[pP]>\\|<[pP][^>]*>"    "\n\n" s))
           (s (replace-regexp-in-string "<[^>]+>"  ""    s))
           (s (replace-regexp-in-string "&nbsp;"   " "   s))
           (s (replace-regexp-in-string "&lt;"     "<"   s))
           (s (replace-regexp-in-string "&gt;"     ">"   s))
           (s (replace-regexp-in-string "&amp;"    "&"   s))
           (s (replace-regexp-in-string "&quot;"   "\"" s))
           (s (replace-regexp-in-string "&apos;"   "'"   s)))
      (string-trim s))))

(defun sdcv--html-insert (html &optional _entry _rule)
  "Insert HTML at point, preserving bold, italic, underline, and colors via shr.
Falls back to plain-text tag-stripping when libxml2 is unavailable."
  (if (and (fboundp 'libxml-parse-html-region) (fboundp 'shr-insert-document))
      (let ((tmp    (generate-new-buffer " *sdcv-shr*"))
            (target (current-buffer))
            (width  (max 40 (- (or (and (window-live-p (selected-window))
                                        (window-width))
                                   (frame-width)
                                   80) 4))))
        (unwind-protect
            (progn
              (with-current-buffer tmp
                (insert html)
                (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                       (shr-width          width)
                    (shr-use-fonts      t)
                       (shr-use-colors     sdcv-shr-use-colors)
                       (shr-inhibit-images t)
                       (shr-bullet         "- "))
                  (erase-buffer)
                  ;; Bypass shr-color-visible contrast check so HTML colors
                  ;; are always rendered instead of being silently discarded.
                  (if sdcv-shr-use-colors
                      (cl-letf (((symbol-function 'shr-color-visible)
                                 (lambda (bg fg &optional _mode) (list bg fg))))
                        (shr-insert-document dom))
                    (shr-insert-document dom))
                  ;; Trim trailing blank lines in temp buf before copying
                  (goto-char (point-max))
                  (while (and (> (point) (point-min))
                              (memq (char-before) '(?\n ?\s)))
                    (delete-char -1))))
              ;; Copy content WITH text properties into the target buffer
              (with-current-buffer target
                (insert-buffer-substring tmp)
                (insert "\n")))
          (when (buffer-live-p tmp) (kill-buffer tmp))))
    ;; Fallback: insert stripped plain text
    (insert (sdcv--html-strip html) "\n")))

;;; ── Result rendering ─────────────────────────────────────────────────────────

(defconst sdcv--buffer-name "*sdcv*"
  "Name of the sdcv result buffer.")

(cl-defstruct sdcv-resolution
  "Resolved lookup outcome for a word."
  kind
  results)

(defun sdcv--rule-post-processors (rule)
  "Return RULE's post-processors as a list of functions."
  (let ((value (plist-get rule :post-process)))
    (cond
     ((null value) nil)
     ((functionp value) (list value))
     ((listp value) (seq-filter #'functionp value))
     (t nil))))

(defun sdcv--entry-face-rules (entry rule)
  "Return the face rules applicable to ENTRY under RULE."
  (let* ((dict   (alist-get 'dict entry))
         (rule-faces (plist-get rule :faces))
         (legacy     (cdr (assoc dict sdcv-highlighting-alist #'string=))))
    (append rule-faces
            legacy
            (unless (or rule-faces legacy)
              sdcv-global-highlighting))))

(defun sdcv--face-rule-regexp (rule)
  "Return the regexp part of face RULE."
  (if (and (consp rule)
           (consp (cdr rule))
           (consp (cddr rule))
           (null (cdddr rule))
           (stringp (nth 0 rule))
           (integerp (nth 1 rule)))
      (nth 0 rule)
    (car rule)))

(defun sdcv--face-rule-subexp (rule)
  "Return the subexpression index for face RULE."
  (if (and (consp rule)
           (consp (cdr rule))
           (consp (cddr rule))
           (null (cdddr rule))
           (stringp (nth 0 rule))
           (integerp (nth 1 rule)))
      (or (nth 1 rule) 0)
    0))

(defun sdcv--face-rule-face (rule)
  "Return the face part of face RULE."
  (if (and (consp rule)
           (consp (cdr rule))
           (consp (cddr rule))
           (null (cdddr rule))
           (stringp (nth 0 rule))
           (integerp (nth 1 rule)))
      (nth 2 rule)
    (cdr rule)))

(defun sdcv--apply-face-rules (start end rules)
  "Apply RULES between START and END."
  (let ((priority 950))
    (dolist (rule rules)
    (save-excursion
      (goto-char start)
        (while (re-search-forward (sdcv--face-rule-regexp rule) end t)
          (let* ((subexp     (sdcv--face-rule-subexp rule))
                 (face       (sdcv--face-rule-face rule))
                 (match-beg  (match-beginning subexp))
                 (match-end  (match-end subexp)))
            (when (and match-beg match-end face)
              (sdcv--apply-face-overlay match-beg match-end face priority)))))
      (setq priority (1- priority)))))

(defvar sdcv--marginalia-candidates nil
  "Dynamic alist mapping minibuffer candidates to sdcv entries.
Used so Marginalia can annotate grouped selection candidates with unique labels.")

(defun sdcv--inline-annotations-enabled-p ()
  "Return non-nil when sdcv should provide its own inline annotations.
When Marginalia is active, sdcv defers to Marginalia so category-based
annotations can render in the minibuffer UI without being shadowed." 
  (not (and (featurep 'marginalia)
            (bound-and-true-p marginalia-mode))))

(defun sdcv--candidate-entries (candidate)
  "Return all cached result entries for CANDIDATE."
  (seq-filter (lambda (entry)
                (string= candidate (alist-get 'word entry)))
              sdcv--candidate-results))

(defun sdcv--marginalia-entry (candidate)
  "Return the current sdcv entry for CANDIDATE, if known."
  (or (cdr (assoc candidate sdcv--marginalia-candidates #'string=))
      (car (sdcv--candidate-entries candidate))))

(defun sdcv--candidate-dicts (candidate)
  "Return display names of dictionaries contributing CANDIDATE."
  (if-let ((entry (cdr (assoc candidate sdcv--marginalia-candidates #'string=))))
      (list (sdcv--dict-display-name (alist-get 'dict entry)))
    (seq-uniq
     (mapcar (lambda (entry)
               (sdcv--dict-display-name (alist-get 'dict entry)))
             (sdcv--candidate-entries candidate))
     #'string=)))

(defun sdcv--candidate-preview (candidate)
  "Return the preview string for CANDIDATE from cached entries."
  (when-let ((entry (sdcv--marginalia-entry candidate)))
    (sdcv--entry-preview entry)))

(defun sdcv-marginalia-annotate (candidate)
  "Annotate sdcv completion CANDIDATE for Marginalia."
  (when (and (featurep 'marginalia)
             (fboundp 'marginalia--fields))
    (let ((dicts   (sdcv--candidate-dicts candidate))
          (preview (sdcv--candidate-preview candidate)))
      (marginalia--fields
       ((when dicts
          (mapconcat #'identity dicts ", "))
        :face 'marginalia-type :truncate 0.35)
       (preview :face 'marginalia-documentation :truncate 1.0)))))

(defun sdcv--register-marginalia ()
  "Register `sdcv-word' completion metadata with Marginalia."
  (when (boundp 'marginalia-command-categories)
    (dolist (command '(sdcv-lookup sdcv-lookup-select-dicts))
      (setf (alist-get command marginalia-command-categories) 'sdcv-word)))
  (when (boundp 'marginalia-annotators)
    (setf (alist-get 'sdcv-word marginalia-annotators)
          '(sdcv-marginalia-annotate builtin none))))

(with-eval-after-load 'marginalia
  (sdcv--register-marginalia))

(defun sdcv--definition-preview (definition entry rule)
  "Return a plain preview string for DEFINITION in ENTRY according to RULE."
  (let* ((definition (sdcv--transform-definition definition entry rule))
         (format     (sdcv--definition-format definition rule))
         (plain-fn   (or (plist-get rule :plain)
                         (alist-get format sdcv--definition-plain-renderers nil nil #'eq)
                         #'sdcv--text-plain)))
    (funcall plain-fn definition entry rule)))

(defun sdcv--insert-definition (definition entry rule)
  "Insert DEFINITION for ENTRY according to RULE."
  (let* ((definition (sdcv--transform-definition definition entry rule))
         (format     (sdcv--definition-format definition rule))
         (renderer   (or (plist-get rule :renderer)
                         (alist-get format sdcv--definition-renderers nil nil #'eq)
                         #'sdcv--text-insert)))
    (funcall renderer definition entry rule)))

(defun sdcv--entry-preview (entry)
  "Return a one-line preview string for ENTRY, or nil when unavailable."
  (when-let* ((definition (alist-get 'definition entry))
              (rule       (sdcv--rendering-rule (alist-get 'dict entry)))
              (plain      (sdcv--definition-preview definition entry rule)))
    (let* ((oneline (replace-regexp-in-string
                     "[[:space:]]+" " " (string-trim plain)))
           (limit   (min sdcv-preview-chars (length oneline))))
      (unless (string-empty-p oneline)
        (substring oneline 0 limit)))))

(defun sdcv--render-entry (word entry)
  "Insert a rendered section for ENTRY looked up as WORD."
  (let* ((dict          (alist-get 'dict entry))
         (found         (alist-get 'word entry))
         (definition    (alist-get 'definition entry))
         (pref          (sdcv--dict-plist dict))
         (display-name  (sdcv--dict-display-name dict))
         (color         (plist-get pref :color))
         (rule          (sdcv--rendering-rule dict))
         (section-start (copy-marker (point))))
    (let ((heading-start (point)))
      (insert (format "* %s" display-name))
      (sdcv--apply-heading-overlay heading-start (point) color)
      (insert "\n"))
    (when (and found (not (string= found word)))
      (insert (format "  /Found as: %s/\n" found)))
    (insert "\n")
    (condition-case err
        (if definition
            (sdcv--insert-definition definition entry rule)
          (insert "/No definition available./\n"))
      (error
       (insert (sdcv--definition-preview (or definition "") entry rule) "\n")
       (message "sdcv: render error in %s: %s"
                dict (error-message-string err))))
    (insert "\n")
    (let ((section-end (copy-marker (point) t)))
      (dolist (processor (sdcv--rule-post-processors rule))
        (funcall processor section-start section-end entry rule))
      ;; For HTML entries, promote shr bold/italic text-props to overlays so
      ;; they survive org-mode's font-lock refontification.  Done AFTER
      ;; post-processors so that any text rewriting (e.g. synonym-listify)
      ;; doesn't destroy overlays that were applied earlier.
      (when (and definition
                 (eq (sdcv--definition-format definition rule) 'html))
        (sdcv--apply-shr-face-overlays section-start section-end entry rule))
      (sdcv--apply-face-rules section-start section-end
                              (sdcv--entry-face-rules entry rule))
      (set-marker section-start nil)
      (set-marker section-end nil))))

(defun sdcv--render (word results &optional kind)
  "Insert formatted RESULTS for WORD into the current buffer.
KIND describes how the lookup resolved: `exact', `suggestion',
`alternatives', or `empty'."
  (let ((inhibit-read-only t))
    (sdcv--clear-style-overlays)
    (erase-buffer)
    (insert (format "#+TITLE: %s\n\n" word))
    (pcase kind
      ('suggestion
       (insert (format "/No exact match for \"%s\". Showing the closest hit./\n\n"
                       word)))
      ('alternatives
       (insert (format "/No exact match for \"%s\". Showing alternative hits./\n\n"
                       word))))
    (if (null results)
        (insert (format "/No results found for \"%s\"./\n" word))
      (mapc (lambda (entry) (sdcv--render-entry word entry)) results))))

;;; ── Async query ─────────────────────────────────────────────────────────────

(defvar sdcv--pending-process nil
  "Currently in-flight async sdcv lookup process, or nil.")

(cl-defun sdcv--query-async (word callback &key dicts exact-search)
  "Look up WORD asynchronously and call CALLBACK with parsed results.
Restrict the lookup to DICTS when non-nil.  When EXACT-SEARCH is non-nil, only
exact headword hits are returned.  Any in-flight lookup is cancelled first."
  (when (and sdcv--pending-process (process-live-p sdcv--pending-process))
    (delete-process sdcv--pending-process)
    (setq sdcv--pending-process nil))
  (let* ((args (sdcv--query-args word dicts exact-search))
         (acc  "")
         (proc (apply #'make-process
                      (list :name     "sdcv-lookup"
                            :buffer   nil
                            :command  (cons sdcv-program args)
                            :filter   (lambda (_p chunk)
                                        (setq acc (concat acc chunk)))
                            :sentinel (lambda (process _event)
                                        (when (memq (process-status process) '(exit signal))
                                          (setq sdcv--pending-process nil)
                                          (funcall callback
                                                   (and (zerop (process-exit-status process))
                                                        (sdcv--parse-json-results acc)))))))))
    (setq sdcv--pending-process proc)
    proc))

(defun sdcv--pattern-query-p (word)
  "Return non-nil when WORD should skip exact-first lookup.
Pattern queries contain wildcard or whitespace syntax."
  (string-match-p "[*?[:space:]]" (or word "")))

(defun sdcv--resolve-results (exact-results fuzzy-results)
  "Resolve EXACT-RESULTS and FUZZY-RESULTS into a lookup outcome."
  (cond
   (exact-results
    (make-sdcv-resolution :kind 'exact :results exact-results))
   ((null fuzzy-results)
    (make-sdcv-resolution :kind 'empty :results nil))
   ((cdr fuzzy-results)
    (make-sdcv-resolution :kind 'alternatives :results fuzzy-results))
   (t
    (make-sdcv-resolution :kind 'suggestion :results fuzzy-results))))

(cl-defun sdcv--resolve-word-async (word callback &key dicts)
  "Resolve WORD asynchronously and pass a `sdcv-resolution' to CALLBACK."
  (let ((finish (lambda (exact-results fuzzy-results)
                  (funcall callback
                           (sdcv--resolve-results exact-results fuzzy-results)))))
    (if (sdcv--pattern-query-p word)
        (sdcv--query-async word
                           (lambda (results)
                             (funcall finish nil results))
                           :dicts dicts)
      (sdcv--query-async
       word
       (lambda (exact-results)
         (if exact-results
             (funcall finish exact-results nil)
           (sdcv--query-async word
                              (lambda (fuzzy-results)
                                (funcall finish nil fuzzy-results))
                              :dicts dicts)))
       :dicts dicts
       :exact-search t))))

;;; ── Navigation history (buffer-local) ───────────────────────────────────────

(defvar-local sdcv--nav-history nil
  "Buffer-local navigation history: list of words, newest first.")

(defvar-local sdcv--nav-pos 0
  "Current position in `sdcv--nav-history' (0 = most recent).")

(defun sdcv--nav-push (word)
  "Push WORD onto the buffer-local navigation history, truncating forward."
  ;; Truncate any forward entries
  (when (> sdcv--nav-pos 0)
    (setq sdcv--nav-history (nthcdr sdcv--nav-pos sdcv--nav-history))
    (setq sdcv--nav-pos 0))
  (unless (string= word (car sdcv--nav-history))
    (push word sdcv--nav-history)))

;;; ── Display ─────────────────────────────────────────────────────────────────

(defun sdcv--fill-buffer (buf word entries &optional kind)
  "Fill BUF with ENTRIES for WORD; set up modes and make read-only.
KIND describes the resolved lookup outcome."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (read-only-mode -1)
      (let ((inhibit-read-only t))
        (sdcv--render word entries kind)
        (unless (derived-mode-p 'org-mode) (org-mode))
        (sdcv-mode 1)
        (when sdcv-auto-org-entities (sdcv-apply-org-entities))
        (set-buffer-modified-p nil))
      (read-only-mode 1)
      (sdcv--show-buffer-start buf))))

;; Keep old name as alias for test compatibility.
(defalias 'sdcv--display-finalize #'sdcv--fill-buffer)

(defun sdcv--show-loading-buffer (buf word &optional no-nav-push)
  "Show BUF with a loading indicator for WORD.
Unless NO-NAV-PUSH is non-nil, push WORD onto the buffer-local history."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (unless no-nav-push (sdcv--nav-push word))
      (read-only-mode -1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s\n\n/Loading.../\n" word))
        (unless (derived-mode-p 'org-mode) (org-mode))
        (sdcv-mode 1)
        (set-buffer-modified-p nil))
      (read-only-mode 1)
      (sdcv--show-buffer-start buf))
    (pop-to-buffer buf)
    (sdcv--show-buffer-start buf)))

(defun sdcv--present-buffer (buf word entries kind &optional no-nav-push)
  "Fill BUF with ENTRIES for WORD, then display it.
KIND describes the resolved lookup outcome.  Unless NO-NAV-PUSH is non-nil,
push WORD onto the buffer-local history before showing the buffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (unless no-nav-push (sdcv--nav-push word))))
  (sdcv--fill-buffer buf word entries kind)
  (pop-to-buffer buf)
  (sdcv--show-buffer-start buf))

;;; ── Static result selection ──────────────────────────────────────────────────

(defun sdcv--selection-choices (results)
  "Return an alist of unique completion labels to RESULT entries."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar
     (lambda (entry)
       (let* ((word  (or (alist-get 'word entry) "?"))
              (count (1+ (gethash word seen 0)))
              (label (if (= count 1)
                         word
                       (format "%s [%d]" word count))))
         (puthash word count seen)
         (cons label entry)))
     results)))

(defun sdcv--make-selection-table (results)
  "Build a standard completion table for RESULTS with dict group headers."
  (let* ((choices (sdcv--selection-choices results))
         (labels  (mapcar #'car choices))
         (lookup  (lambda (label) (cdr (assoc label choices #'string=))))
         (group-fn
          (lambda (cand transform)
            (if transform
                cand
              (when-let ((entry (funcall lookup cand)))
                (sdcv--dict-display-name (alist-get 'dict entry))))))
         (annotation-fn
          (lambda (cand)
            (when-let ((entry (funcall lookup cand)))
              (let* ((dict    (sdcv--dict-display-name (alist-get 'dict entry)))
                     (preview (sdcv--entry-preview entry)))
                (concat
                 (propertize (concat "  " dict) 'face 'marginalia-type)
                 (when preview
                   (concat "  "
                           (propertize preview 'face 'completions-annotations))))))))
         (metadata
          `((category              . sdcv-word)
            (display-sort-function . identity)
            (cycle-sort-function   . identity)
            (group-function        . ,group-fn))))
    (when (sdcv--inline-annotations-enabled-p)
      (setq metadata
            (append metadata `((annotation-function . ,annotation-fn)))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata ,@metadata)
        (complete-with-action action labels str pred)))))

(defun sdcv--select-result (word results)
  "Prompt user to pick one entry from RESULTS for WORD; return its alist.
Skips the prompt and returns the sole entry when RESULTS has one element.
If the user quits (C-g), fall back to returning the whole list (nil = fallback)."
  (cond
   ((null results)         nil)
   ((= 1 (length results)) (car results))
   (t
    (let* ((choices (sdcv--selection-choices results))
           (table  (sdcv--make-selection-table results))
          (sel   (let ((sdcv--marginalia-candidates choices))
                (condition-case nil
                  (completing-read
                   (format "Results for \"%s\": " word)
                   table nil t nil nil nil)
                 (quit nil)))))
      (when sel
        (cdr (assoc sel choices #'string=)))))))

(cl-defun sdcv--display-word (word &key dicts no-nav-push allow-selector)
  "Look up WORD asynchronously and display in `sdcv--buffer-name'.
Unless NO-NAV-PUSH is non-nil, push WORD onto the buffer-local navigation
history.  A \"Loading...\" indicator is shown only when ALLOW-SELECTOR is nil
(e.g. during history navigation).

When ALLOW-SELECTOR is non-nil the display style is controlled by
`sdcv-multi-result-style':
  `all-in-one'  All entries are placed into one result buffer.
  `selector'    A grouped `completing-read' prompt is shown whenever there
                are multiple entries OR the lookup kind is `alternatives'.
                Single-entry results are always shown directly."
  (sdcv--history-push word)
  (let* ((buf          (get-buffer-create sdcv--buffer-name))
         (show-loading (not allow-selector)))
    (when show-loading
      (sdcv--show-loading-buffer buf word no-nav-push))
    (sdcv--resolve-word-async
     word
     (lambda (resolution)
      (let* ((kind    (sdcv-resolution-kind resolution))
         (results (sdcv--sort-results
               (sdcv-resolution-results resolution)
               dicts)))
         (if (and allow-selector
                  (or (eq kind 'alternatives)
                      (and (eq sdcv-multi-result-style 'selector)
                           (cdr results))))
             ;; `completing-read' cannot safely run from the process sentinel.
             (run-with-idle-timer
              0 nil
              (lambda ()
                (let ((entry (sdcv--select-result word results)))
                  (sdcv--present-buffer buf word (if entry (list entry) results)
                                        kind no-nav-push))))
           (if show-loading
               (sdcv--fill-buffer buf word results kind)
             (sdcv--present-buffer buf word results kind no-nav-push)))))
     :dicts dicts)
    buf))

;;; ── Completion ───────────────────────────────────────────────────────────────

(defvar sdcv--candidate-results nil
  "Full result alists from the most recent `sdcv--candidates' call.
Used by `sdcv--annotation' to avoid extra queries per candidate.")

(defvar sdcv--completion-cache (cons nil nil)
  "Cache of ((QUERY DICTS) . WORDS) for `sdcv--candidates'.")

(defun sdcv-default-completion-query (input)
  "Build a completion query for INPUT.
Explicit `*' and `?' wildcards are preserved.  Plain input is turned into a
prefix query by appending `*'.  When INPUT contains multiple space-separated
components, only the first one is sent to sdcv so other completion styles can
filter client-side."
  (let ((token (car (split-string (or input "") "[[:space:]]+" t))))
    (cond
     ((null token) nil)
     ((string-match-p "[*?]" token) token)
     ((< (length token) sdcv-completion-min-input) nil)
     (t (concat token "*")))))

(defun sdcv--candidates (input &optional dicts)
  "Return a list of matching word strings for INPUT from sdcv.
Always uses `sdcv--query-direct' so completion remains synchronous and stable
inside the minibuffer.  Results are cached per (INPUT DICTS) pair."
  (let ((cache-key (list input dicts)))
    (cond
     ((null input)
      (setq sdcv--candidate-results nil)
      nil)
     ((equal cache-key (car sdcv--completion-cache))
      (cdr sdcv--completion-cache))
     (t
      (let* ((results (sdcv--query-direct input :dicts dicts))
             (words   (seq-uniq
                       (delq nil (mapcar (lambda (entry) (alist-get 'word entry))
                                         results))
                       #'string=)))
        (setq sdcv--candidate-results results
              sdcv--completion-cache (cons cache-key words))
        words)))))

(defun sdcv--annotation (candidate)
  "Return a preview annotation for CANDIDATE using cached result data."
  (when-let ((preview (sdcv--candidate-preview candidate)))
  (concat "  " (propertize preview 'face 'completions-annotations))))

(defun sdcv--completion-annotation-string (candidate)
  "Return a completion annotation string for CANDIDATE."
  (or (and (featurep 'marginalia)
           (fboundp 'marginalia--fields)
           (sdcv-marginalia-annotate candidate))
      (sdcv--annotation candidate)
      ""))

(defun sdcv--affixate (candidates)
  "Return affixated CANDIDATES for completion UIs such as Vertico."
  (mapcar (lambda (candidate)
            (list candidate "" (sdcv--completion-annotation-string candidate)))
          candidates))

(defun sdcv--completion-metadata ()
  "Return metadata for sdcv completion tables."
  (let ((metadata '((category            . sdcv-word)
                    (affixation-function . sdcv--affixate))))
    (when (sdcv--inline-annotations-enabled-p)
      (setq metadata
            (append metadata '((annotation-function . sdcv--annotation)))))
    metadata))

(defun sdcv--completion-extra-properties ()
  "Return extra completion properties for interactive sdcv prompts."
  (let ((properties '(:category sdcv-word
                      :affixation-function sdcv--affixate)))
    (when (sdcv--inline-annotations-enabled-p)
      (setq properties
            (plist-put properties :annotation-function #'sdcv--annotation)))
    properties))

(defun sdcv--completion-table-for (&optional dicts)
  "Return a dynamic completion table restricted to DICTS."
  (let ((dynamic-table
         (completion-table-dynamic
          (lambda (str)
            (let* ((builder (or sdcv-completion-query-function
                                #'sdcv-default-completion-query))
                   (query   (funcall builder str)))
              (or (sdcv--candidates query dicts) '()))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata ,@(sdcv--completion-metadata))
        (funcall dynamic-table str pred action)))))

(defun sdcv--completion-table (str pred action)
  "Backward-compatible wrapper around `sdcv--completion-table-for'."
  (funcall (sdcv--completion-table-for nil) str pred action))

;;; ── Interactive commands ─────────────────────────────────────────────────────

;;;###autoload
(defun sdcv-lookup-word-at-point ()
  "Look up the word at point.
Exact hits are displayed immediately.  Fuzzy alternatives are offered through a
grouped completion prompt."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (if word
        (sdcv--display-word word :allow-selector t)
      (message "sdcv: no word at point"))))

;;;###autoload
(defun sdcv-lookup (&optional dicts)
  "Dynamic word lookup with live minibuffer completion.
With DICTS (list of names), restrict search to those dictionaries."
  (interactive)
  (let* ((default (thing-at-point 'word t))
         (prompt  (if default
                      (format "Look up [%s]: " default)
                    "Look up: "))
         (word    (let ((completion-extra-properties
                         (sdcv--completion-extra-properties)))
                    (completing-read prompt
                                     (sdcv--completion-table-for dicts)
                                     nil nil nil
                                     'sdcv-history
                                     default))))
    (when (and word (not (string-empty-p word)))
      (sdcv--display-word word :dicts dicts :allow-selector t))))

;;;###autoload
(defun sdcv-lookup-select (&optional dicts)
  "Prompt for a word and resolve it like `sdcv-lookup-word-at-point'.
With DICTS (list of names), restrict search to those dictionaries."
  (interactive)
  (let* ((default (thing-at-point 'word t))
         (word    (read-string
                   (if default (format "Look up [%s]: " default) "Look up: ")
                   nil 'sdcv-history default)))
    (when (and word (not (string-empty-p word)))
      (sdcv--display-word word :dicts dicts :allow-selector t))))

;;;###autoload
(defun sdcv-lookup-all-dicts ()
  "Look up a word using ALL available dictionaries."
  (interactive)
  (let* ((all  (sdcv-available-dicts))
         (word (or (thing-at-point 'word t)
                   (read-string "Look up (all dicts): "))))
    (when (and word (not (string-empty-p word)))
      (sdcv--display-word word :dicts all :allow-selector t))))

;;;###autoload
(defun sdcv-lookup-select-dicts ()
  "Select dictionaries interactively, then perform a word lookup."
  (interactive)
  (let ((dicts (sdcv-select-dicts)))
    (sdcv-lookup dicts)))

;;; ── sdcv-mode minor mode ─────────────────────────────────────────────────────

(defun sdcv-quit ()
  "Close the sdcv result buffer."
  (interactive)
  (quit-window t))

(defun sdcv-next-section ()
  "Move point to the next dictionary section (org heading)."
  (interactive)
  (org-next-visible-heading 1))

(defun sdcv-prev-section ()
  "Move point to the previous dictionary section (org heading)."
  (interactive)
  (org-previous-visible-heading 1))

(defun sdcv-history-back ()
  "Navigate back in the sdcv buffer's visit history."
  (interactive)
  (let* ((hist sdcv--nav-history)
         (new-pos (1+ sdcv--nav-pos))
         (max-pos (1- (length hist))))
    (if (> new-pos max-pos)
        (message "sdcv: beginning of history")
      (setq sdcv--nav-pos new-pos)
      ;; History nav always shows all results; no selector needed.
      (sdcv--display-word (nth new-pos hist)
                          :no-nav-push t
                          :allow-selector nil))))

(defun sdcv-history-forward ()
  "Navigate forward in the sdcv buffer's visit history."
  (interactive)
  (if (<= sdcv--nav-pos 0)
      (message "sdcv: end of history")
    (setq sdcv--nav-pos (1- sdcv--nav-pos))
    (sdcv--display-word (nth sdcv--nav-pos sdcv--nav-history)
                        :no-nav-push t
                        :allow-selector nil)))

(defun sdcv-lookup-word-at-point-recursive ()
  "Look up word at point within the sdcv buffer, adding to history.
Shows grouped selector when multiple results are found."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (if word
        (sdcv--display-word word :allow-selector t)
      (message "sdcv: no word at point"))))

(defvar sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")   #'sdcv-quit)
    (define-key map (kbd "n")   #'sdcv-next-section)
    (define-key map (kbd "p")   #'sdcv-prev-section)
    (define-key map (kbd "r")   #'sdcv-lookup-select)    ; static selector prompt
    (define-key map (kbd "R")   #'sdcv-lookup)           ; dynamic live completion
    (define-key map (kbd "l")   #'sdcv-history-back)
    (define-key map (kbd "f")   #'sdcv-history-forward)
    (define-key map (kbd "e")   #'sdcv-apply-org-entities)
    (define-key map (kbd "RET") #'sdcv-lookup-word-at-point-recursive)
    (define-key map [mouse-1]   #'sdcv-lookup-word-at-point-recursive)
    map)
  "Keymap for `sdcv-mode'.")

;;;###autoload
(define-minor-mode sdcv-mode
  "Minor mode active in sdcv result buffers.

Key bindings:
  r  — prompt for a word with grouped result selector  (`sdcv-lookup-select')
  R  — dynamic live-completion lookup                  (`sdcv-lookup')
  RET/click — look up word at point, with selector     (`sdcv-lookup-word-at-point-recursive')
  n/p — next/previous dictionary heading
  l/f — history back / forward
  e   — replace ASCII with org-entities
  q   — close buffer
  \\{sdcv-mode-map}"
  :lighter " sdcv"
  :keymap sdcv-mode-map
  (if sdcv-mode
      (progn
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (visual-line-mode -1)))

;;; ── Suggested global bindings (commented out, user configures) ───────────────

;; (global-set-key (kbd "C-c d")   #'sdcv-lookup)
;; (global-set-key (kbd "C-c D")   #'sdcv-lookup-word-at-point)
;; (global-set-key (kbd "C-c M-d") #'sdcv-lookup-all-dicts)

(provide 'sdcv)
;;; sdcv.el ends here
