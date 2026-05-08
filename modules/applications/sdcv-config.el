;;; sdcv-config.el --- Personal configuration for sdcv.el -*- lexical-binding: t; -*-

;;; Code:

;; Dictionaries live in ~/.emacs.d/dicts/ (nested subdirectories)
(setq sdcv-data-dir (expand-file-name "dicts" user-emacs-directory))
(setq sdcv-only-data-dir t)   ; ignore system-wide dicts

;; all-in-one all search results will get rendered in the same buffer, grouped by dictionary
;; selector use `completing-read' to choose which dict entry to jump to when multiple results are found
(setq sdcv-multi-result-style 'all-in-one)

;; Preferred dictionaries – order determines lookup priority, display order,
;; and heading order.
;; :name    must match exactly the bookname in the .ifo file
;; :display human-readable label shown in org headings and completion UIs
;; :color   optional face color for the org heading
(setq sdcv-preferred-dicts
      '((:name    "Duden – Deutsches Universalwörterbuch"
         :display "Duden DE"
         :color   "cornflower blue")
        (:name    "Duden – Das Synonymwörterbuch"
         :display "Duden Synonym"
         :color   "medium sea green")
        (:name    "Duden – Das Fremdwörterbuch"
         :display "Duden Fremd"
         :color   "orchid")
        (:name    "PONS Universelles Wörterbuch Englisch-Deutsch"
         :display "PONS EN→DE"
         :color   "goldenrod")
        (:name    "PONS Universelles Wörterbuch Deutsch-Englisch"
         :display "PONS DE→EN"
         :color   "sandy brown")
        (:name    "Babylon English-German"
         :display "Babylon EN-DE"
         :color   "steel blue")))

;; Show up to 100 chars in the completion annotation preview
(setq sdcv-preview-chars 100)

(defface sdcv-duden-headword-face
      '((t :foreground "tomato" :weight bold :height 1.1))
      "Face used for rendered Duden headwords and their inflected forms."
      :group 'sdcv)

(defface sdcv-duden-meta-face
      '((t :inherit font-lock-type-face))
      "Face used for rendered Duden grammar and morphology metadata."
      :group 'sdcv)

(defface sdcv-duden-sense-face
      '((t :inherit font-lock-constant-face :weight bold))
      "Face used for rendered Duden sense markers."
      :group 'sdcv)

(defface sdcv-duden-phrase-face
      '((t :foreground "medium orchid" :weight semibold))
      "Face used for rendered Duden idioms and fixed phrases."
      :group 'sdcv)

(defface sdcv-duden-example-face
      '((t :slant italic))
      "Face used for rendered Duden examples."
      :group 'sdcv)

(defface sdcv-duden-abbreviation-face
      '((t :foreground "gold"))
      "Face for Duden usage/register abbreviations (jmdm., etw., ugs., …)."
      :group 'sdcv)

(defface sdcv-duden-quote-background-face
      '((((background dark))  :background "#2d2d10" :extend t)
        (((background light)) :background "#fffff0" :extend t))
      "Background face applied to Duden #+begin_quote … #+end_quote blocks."
      :group 'sdcv)

(defvar sdcv--duden-headword nil
      "Currently rendered Duden headword.")

(defun sdcv--duden-expand-short-form (text)
      "Expand single-letter Duden headword abbreviations inside TEXT."
      (if-let* ((word sdcv--duden-headword)
                                    (initial (and (> (length word) 1)
                                                                              (regexp-quote (substring (downcase word) 0 1)))))
                  (replace-regexp-in-string
                   (format "\\(^\\|[[:space:](\\[\"'/*~]\\)%s\\.\\([[:space:],;:!?)]\\|[*~/]\\|$\\)"
                                           initial)
                   (concat "\\1" word "\\2")
                   text t nil)
            text))

(defun sdcv--duden-expand-quote-shortcuts (text)
      "Expand Duden shorthand forms inside example quote TEXT.
This keeps the expansion Duden-specific by only touching quote content."
      (let ((expanded (sdcv--duden-expand-short-form text)))
            (if-let ((word sdcv--duden-headword))
                    (replace-regexp-in-string
                     "\\(^\\|[[:space:](\\[\"'/*~]\\)-\\([[:alpha:]][[:alpha:]]*\\)\\([[:space:],;:!?)]\\|[*~/]\\|$\\)"
                     (lambda (match)
                           (when (string-match
                                          "\\(^\\|[[:space:](\\[\"'/*~]\\)-\\([[:alpha:]][[:alpha:]]*\\)\\([[:space:],;:!?)]\\|[*~/]\\|$\\)"
                                          match)
                                 (concat (match-string 1 match)
                                                 word
                                                 (match-string 2 match)
                                                 (match-string 3 match))))
                     expanded t nil)
                  expanded)))

(defun sdcv--duden-expand-suffix-forms (start end entry _rule)
      "Expand Duden -suffix shorthand forms outside bsptext spans between START and END.
Forms like -en or -ist that were not expanded during HTML rendering (i.e. they
appear outside #+begin_quote blocks) are concatenated with the headword here."
      (when-let ((word (alist-get 'word entry)))
            (let ((sdcv--duden-headword word))
                  (save-excursion
                        (goto-char start)
                        (while (re-search-forward
                                        "\\([[:space:](\\[\"'/*~]\\)-\\([[:alpha:]][[:alpha:]]*\\)\\([[:space:],;:!?)]\\|[*~/]\\|$\\)"
                                        end t)
                              (replace-match
                               (concat (match-string 1) word (match-string 2) (match-string 3))
                               t t))))))

(defun sdcv--duden-highlight-quote-blocks (start end _entry _rule)
      "Apply a background highlight to #+begin_quote … #+end_quote blocks."
      (save-excursion
            (goto-char start)
            (while (re-search-forward "^#\\+begin_quote$" end t)
                  (let ((block-start (match-beginning 0)))
                        (when (re-search-forward "^#\\+end_quote$" end t)
                              (sdcv--apply-face-overlay block-start (match-end 0)
                                                                             'sdcv-duden-quote-background-face 800))))))

(defun sdcv--duden-highlight-headword-occurrences (start end entry _rule)
      "Highlight all inline occurrences of the headword and its suffix-inflected forms.
This makes forms expanded from -suffix notation visually match the main headword."
      (when-let ((word (alist-get 'word entry)))
            (save-excursion
                  (goto-char start)
                  (while (re-search-forward
                                  (concat "\\b" (regexp-quote word) "[[:alpha:]]*\\b")
                                  end t)
                        (sdcv--apply-face-overlay (match-beginning 0) (match-end 0)
                                                                         'sdcv-duden-headword-face 960)))))

(defun sdcv--duden-polish-section (start end _entry _rule)
      "Normalize rendered Duden output between START and END."
      (let ((content (buffer-substring-no-properties start end))
                        (in-quote nil)
                        lines)
            (dolist (line (split-string content "\n" nil))
                  (cond
                   ((string= line "#+begin_quote")
                        (push line lines)
                        (setq in-quote t))
                   ((string= line "#+end_quote")
                        (push line lines)
                        (setq in-quote nil))
                   ((or in-quote
                                    (string-empty-p (string-trim line))
                                    (string-match-p "\\`#+" line)
                                    (string-match-p "\\`[0-9]+\\." line)
                                    (string-match-p "\\`\\* " line)
                                    (string-match-p "\\`\\*[^*\n]+\\*\\(?:[[:space:]]+~<[^>\n]+>~\\)?\\'" line)
                                    (string-match-p "\\`[[:space:]]*[-+] " line))
                        (push line lines))
                   (t
                        (let ((normalized (replace-regexp-in-string "\\`\\*\\*\\* +" "" line)))
                              (if (or (string-match-p "\\`\\*" normalized)
                                                      (string-match-p "\\`\\*\\*\\* +" line))
                                          (push (concat "  - " normalized) lines)
                                    (push normalized lines))))))
            (save-excursion
                  (delete-region start end)
                  (goto-char start)
                  (insert (mapconcat #'identity (nreverse lines) "\n")))))

(defun sdcv--duden-node-text (node)
      "Return the concatenated plain text content of DOM NODE."
      (cond ((stringp node) node)
                        ((consp node) (apply #'concat (mapcar #'sdcv--duden-node-text (cddr node))))
                        (t "")))

(defun sdcv--duden-walk (node)
      "Walk Duden StarDict DOM NODE, inserting org-oriented text at point."
      (cond
       ((stringp node) (insert node))
       ((consp node)
            (let* ((tag   (car node))
                               (attrs (cadr node))
                               (kids  (cddr node)))
                  (pcase tag
                        ((or 'html 'head 'body) (mapc #'sdcv--duden-walk kids))
                        ('br (insert "\n"))
                        ('b
                         (let ((sub (string-trim (apply #'concat (mapcar #'sdcv--duden-node-text kids)))))
                               (cond
                                    ((string-empty-p sub) nil)
                                    ((string= sub "*")
                                     (unless (bolp) (insert "\n"))
                                     (insert "  - "))
                                    ((string-match-p "\\`[a-z])\\'" sub)
                                     (insert "\n  - " sub " "))
                                    (t
                                     (unless (sdcv--org-pre-boundary-p) (insert " "))
                                     (let ((s (point)))
                                           (insert sub)
                                           (add-text-properties
                                            s (point)
                                            (if (string-match-p " " sub)
                                                    '(face (:weight bold) sdcv-phrase t)
                                                  '(face (:weight bold)))))))))
                        ('i
                         (let* ((s0 (point))
                                            (_ (mapc #'sdcv--duden-walk kids))
                                            (trimmed (string-trim
                                                                      (buffer-substring-no-properties s0 (point)))))
                               (delete-region s0 (point))
                               (unless (string-empty-p trimmed)
                                     (unless (sdcv--org-pre-boundary-p) (insert " "))
                                     (let ((s (point)))
                                           (insert trimmed)
                                           (put-text-property s (point) 'face '(:slant italic))))))
                        ('u (mapc #'sdcv--duden-walk kids))
                        ('img nil)
                        ('a (mapc #'sdcv--duden-walk kids))
                        ('font
                         (let* ((color (cdr (assq 'color attrs)))
                                                (text  (string-trim
                                                                        (apply #'concat (mapcar #'sdcv--duden-node-text kids)))))
                               (if (and color
                                                            (member (downcase color)
                                                                                    '("blue" "0000ff" "#0000ff" "3333cc" "#3333cc"))
                                                            (string-match-p "\\`[0-9]+\\.\\'" text))
                                           (progn
                                                 (insert "\n")
                                                 (insert text)
                                                 (insert " "))
                                     (mapc #'sdcv--duden-walk kids))))
                        ('span
                         (let ((class (cdr (assq 'class attrs)))
                                           (style (or (cdr (assq 'style attrs)) "")))
                               (cond
                                    ((equal class "bsp")
                                     (insert "\n#+begin_quote\n")
                                     (mapc #'sdcv--duden-walk kids)
                                     (unless (bolp) (insert "\n"))
                                     (insert "#+end_quote\n"))
                                    ((equal class "bsptext")
                                     (let ((sub (with-temp-buffer
                                                                              (mapc #'sdcv--duden-walk kids)
                                                                              (sdcv--cleanup-rendered-buffer))))
                                           (unless (string-empty-p sub)
                                                 (insert "- " (sdcv--duden-expand-quote-shortcuts sub))
                                                 (unless (bolp) (insert "\n")))))
                                    ((equal class "meta")
                                     (let ((sub (with-temp-buffer
                                                                              (mapc #'sdcv--duden-walk kids)
                                                                              (string-trim (buffer-string)))))
                                           (unless (string-empty-p sub)
                                                 (sdcv--insert-org-wrapper "~" sub))))
                                    ((equal class "prag")
                                     (mapc #'sdcv--duden-walk kids))
                                    ((string-match-p "font-weight[ \t]*:[ \t]*normal" style)
                                     (mapc #'sdcv--duden-walk kids))
                                    (t (mapc #'sdcv--duden-walk kids)))))
                        (_ (mapc #'sdcv--duden-walk kids)))))))

(defun sdcv--duden-insert (html &optional entry _rule)
      "Insert Duden StarDict HTML as formatted text, preserving face properties.
EXTRY provides the headword for expanding Duden short forms."
      (if (not (fboundp 'libxml-parse-html-region))
                  (insert (sdcv--html-strip html entry) "\n")
            (let ((sdcv--duden-headword (alist-get 'word entry))
                              (target (current-buffer))
                              (tmp (generate-new-buffer " *sdcv-duden-tmp*")))
                  (unwind-protect
                          (progn
                                (with-current-buffer tmp
                                      (insert html)
                                      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
                                            (erase-buffer)
                                            (sdcv--duden-walk dom))
                                      ;; Normalize in-place, preserving text properties
                                      (goto-char (point-min))
                                      (while (re-search-forward "\n\n\n+" nil t) (replace-match "\n\n"))
                                      (goto-char (point-min))
                                      (while (re-search-forward "[ \t]+$" nil t) (replace-match ""))
                                      ;; Trim leading blank lines/whitespace
                                      (goto-char (point-min))
                                      (skip-chars-forward " \t\n")
                                      (delete-region (point-min) (point))
                                      ;; Trim trailing blank lines/whitespace
                                      (goto-char (point-max))
                                      (skip-chars-backward " \t\n")
                                      (delete-region (point) (point-max)))
                                (with-current-buffer target
                                      (insert-buffer-substring tmp)
                                      (insert "\n")))
                        (when (buffer-live-p tmp) (kill-buffer tmp))))))

(defun sdcv--duden-expand-headword-abbreviations (start end entry _rule)
      "Expand one-letter Duden abbreviations between START and END for ENTRY."
      (when-let* ((word (alist-get 'word entry))
                                          (initial (and (> (length word) 1)
                                                                                    (regexp-quote (substring (downcase word) 0 1)))))
            (save-excursion
                  (goto-char start)
                  (while (re-search-forward
                                          (format "\\(^\\|[[:space:](\\[\"'/*~]\\)%s\\.\\([[:space:],;:!?)]\\|[*~/]\\|$\\)"
                                                                  initial)
                                          end t)
                        (replace-match (concat "\\1" word "\\2") t nil)))))

;;; ── Synonym-dict comma listify ───────────────────────────────────────────────

(defun sdcv--duden-synonym-listify (start end _entry _rule)
  "Reformat Duden Synonym entries in START..END as structured org bullet lists.

Lines of the form 'a) word1, word2, ...' (optionally 'a.) ...') are turned
into a letter heading followed by sorted '  - item' bullets:

  a)
    - darstellen
    - heißen
    - sein

Remaining comma-separated lines (no letter prefix) are sorted the same way.
Numbered header lines ('1.', '2.') and already-formatted lines are untouched."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let* ((bol  (line-beginning-position))
             (eol  (line-end-position))
             (line (buffer-substring-no-properties bol eol))
             skip)
        (cond
         ;; "a) word1, word2, ..."  or  "a.) word1, word2, ..."
         ((string-match "\\`[[:space:]]*\\([a-z]\\)\\.?)\\s-+\\(.*\\)\\'" line)
          (let* ((letter (match-string 1 line))
                 (rest   (string-trim (match-string 2 line))))
            (when (string-match-p "," rest)
              (let* ((items  (split-string rest "[,;]" t "[[:space:]]+"))
                     (valid  (seq-filter (lambda (s) (not (string-empty-p (string-trim s))))
                                         (mapcar #'string-trim items)))
                     (sorted (sort (copy-sequence valid) #'string<)))
                (when (>= (length sorted) 2)
                  (delete-region bol (1+ eol))   ; delete line including \n
                  (insert letter ")\n")
                  (dolist (item sorted)
                    (insert "  - " item "\n"))
                  (setq skip t))))))            ; point is now at start of next line

         ;; Plain comma line without letter or number prefix
         ((and (string-match-p "," line)
               (not (string-match-p "\\`[[:space:]]*[*#0-9]" line))
               (not (string-empty-p (string-trim line))))
          (let* ((items  (split-string line "[,;]" t "[[:space:]]+"))
                 (valid  (seq-filter (lambda (s) (not (string-empty-p (string-trim s))))
                                     (mapcar #'string-trim items)))
                 (sorted (sort (copy-sequence valid) #'string<)))
            (when (>= (length sorted) 2)
              (delete-region bol eol)          ; keep the \n
              (insert (mapconcat (lambda (i) (concat "  - " i)) sorted "\n"))))))

        (unless skip
          (forward-line 1))))))

;; Dictionary-specific renderers.
;; Rules can match by exact :name, by :regexp, or by :keywords.
;; Users can extend this list with their own :renderer, :transform,
;; :post-process, and :faces entries.
(setq sdcv-rendering-rules
      '((:name "Duden – Deutsches Universalwörterbuch"
       :format html
       :renderer sdcv--duden-insert
       :plain sdcv--html-strip
       :post-process (sdcv--duden-expand-suffix-forms
                  sdcv--duden-expand-headword-abbreviations
                  sdcv--duden-polish-section
                  sdcv--duden-highlight-quote-blocks
                  sdcv--duden-highlight-phrases
                  sdcv--duden-highlight-headword-occurrences)
       :faces (("\\(~<[^>\n]+>~\\)" 1 sdcv-duden-meta-face)
             ("^\\([0-9]+\\.\\)" 1 sdcv-duden-sense-face)
             ("^[[:space:]]*-[[:space:]]+\\([a-z])\\)" 1 sdcv-duden-sense-face)
             ;; Abkürzungen aus dem Duden-Abkürzungsverzeichnis → gold
             ("\\(?:^\\|[[:space:]/(]\\)\\(jmd\\.\\|jmdm\\.\\|jmdn\\.\\|jmds\\.\\|etw\\.\\|ugs\\.\\|geh\\.\\|bildl\\.\\|übertr\\.\\|iron\\.\\|scherzh\\.\\|verächtl\\.\\|verhüll\\.\\|landsch\\.\\|bes\\.\\|allg\\.\\|dichter\\.\\|hist\\.\\|fachspr\\.\\|selten\\.\\|veraltend\\.\\|schriftl\\.\\|österr\\.\\|schweiz\\.\\|südd\\.\\|nordd\\.\\)" 1 sdcv-duden-abbreviation-face)
             ("^[[:space:]]*-[[:space:]]+\\([^a-z].*\\|[a-z][^)].*\\)$" 1 sdcv-duden-example-face)))
      (:name "Duden – Das Synonymwörterbuch"
       :format html
       :renderer sdcv--html-insert
       :plain sdcv--html-strip
       :post-process (sdcv--duden-synonym-listify)
       :faces (("\\b[0-9]+\\." . font-lock-constant-face)
             ("\\b[a-z])" . font-lock-keyword-face)
             ("\\bSynonyme?\\b" . font-lock-function-name-face)))
      (:name "Duden – Das Fremdwörterbuch"
       :format html
       :renderer sdcv--html-insert
       :plain sdcv--html-strip
       :faces (("\\b[0-9]+\\." . font-lock-constant-face)
             ("\\b[a-z])" . font-lock-keyword-face)
             ("\\bHerkunft\\b" . font-lock-function-name-face)))
      (:keywords ("PONS")
       :format html
       :renderer sdcv--html-insert
       :plain sdcv--html-strip
       :faces (("\\b\\(sb\\.|sth\\.|etw\\.|jdn\\.|jdm\\.|adj\\.|adv\\.|prep\\.|conj\\.\\)\\b"
              . font-lock-type-face)
             ("\\bto [^;,]+" . font-lock-function-name-face)))
      (:keywords ("Babylon")
       :format html
       :renderer sdcv--html-insert
       :plain sdcv--html-strip
       :faces (("^\\(n\\.|v\\.|adj\\.|adv\\.\\)" . font-lock-keyword-face)
             ("(Comput)" . font-lock-type-face)))))

;; ASCII → Org-Entities: für Dictionary-Inhalte wenig sinnvoll (shr rendert
;; HTML-Entities bereits korrekt).  Im sdcv-Buffer mit `e' manuell aufrufbar.
(setq sdcv-auto-org-entities nil)

;; Entities to keep as plain ASCII (don't replace these)
(setq sdcv-org-entities-blacklist '("pm" "times" "minus"))

;; Legacy per-dict highlighting can stay empty because `sdcv-rendering-rules'
;; now carries the default section-specific face rules.
(setq sdcv-highlighting-alist nil)

;; Global fallback highlighting (applied when no dict-specific rule matches)
(setq sdcv-global-highlighting
      '(("\\b[A-Z][a-z]+\\b" . font-lock-variable-name-face)))

;; Keep the last 200 searches in history
(setq sdcv-history-max 200)

;; Optional key bindings – uncomment to activate
;; (global-set-key (kbd "C-c d")   #'sdcv-lookup)
;; (global-set-key (kbd "C-c D")   #'sdcv-lookup-word-at-point)
;; (global-set-key (kbd "C-c M-d") #'sdcv-lookup-all-dicts)

;;; ── Dictionary toggle UI ─────────────────────────────────────────────────────

(provide 'sdcv-config)
;;; sdcv-config.el ends here
