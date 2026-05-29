;;; sdcv-config.el --- Personal configuration for sdcv.el -*- lexical-binding: t; -*-

;;; Code:

(unless (require 'sdcv nil t)
  (load (expand-file-name "sdcv"
                          (file-name-directory (or load-file-name
                                                   buffer-file-name)))
        nil t))

(unless (require 'sdcv-abbreviations nil t)
  (load (expand-file-name "sdcv-abbreviations"
                          (file-name-directory (or load-file-name
                                                   buffer-file-name)))
        nil t))

(defvar sdcv-duden-abbreviations)
(defvar sdcv-webster-abbreviations)

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
         :color   "slate gray")
        (:name    "Duden – Das Fremdwörterbuch"
         :display "Duden Fremd"
         :color   "orchid")
        (:name    "Duden - Das Herkunftswörterbuch (De-De)"
         :display "Duden Herkunft"
         :color   "dark orange")
        (:name    "Webster's Revised Unabridged Dictionary (1913)"
         :display "Webster"
         :color   "forest green")
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

;; Show expanded dictionary abbreviations inline in the sdcv buffer.
(setq sdcv-show-abbreviation-doc t)

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
      '((((background dark)) :foreground "LightSteelBlue3" :weight semibold)
        (((background light)) :foreground "SteelBlue4" :weight semibold))
      "Face used for rendered Duden idioms and fixed phrases."
      :group 'sdcv)

(defface sdcv-duden-example-face
      '((t :slant italic))
      "Face used for rendered Duden examples."
      :group 'sdcv)

(defface sdcv-duden-reference-face
      '((t :underline t))
      "Face used for Duden cross-reference headwords."
      :group 'sdcv)

(defface sdcv-abbreviation-face
      '((t :foreground "gold"))
      "Face for dictionary usage/register abbreviations."
      :group 'sdcv)

(defface sdcv-abbreviation-link-face
      '((t :inherit link :foreground "deep sky blue" :underline nil))
      "Face used for clickable dictionary abbreviations."
      :group 'sdcv)

(defface sdcv-duden-quote-background-face
      '((((background dark))  :background "#2d2d10" :extend t)
        (((background light)) :background "#fffff0" :extend t))
      "Background face applied to Duden #+begin_quote … #+end_quote blocks."
      :group 'sdcv)

(defface sdcv-duden-idiom-background-face
      '((((background dark))  :background "#283238" :extend t)
        (((background light)) :background "#edf7fa" :extend t))
      "Background face applied to Duden idiom and proverb blocks."
      :group 'sdcv)

(defvar sdcv--duden-headword nil
      "Currently rendered Duden headword.")

(defvar sdcv--duden-phrase-block-open nil
      "Non-nil while rendering a Duden idiom/proverb block.")

(defconst sdcv--duden-short-form-regexp
  "\\(^\\|[[:space:](\\[\"'/*~]\\)%s\\.\\([[:space:],;:!?.()]\\|[*~/]\\|$\\)"
  "Format regexp used to expand one-letter Duden abbreviations.")

(defconst sdcv--duden-suffix-form-regexp
  "\\(^\\|[[:space:](\\[\"'/*~]\\)-\\([[:alpha:]][[:alpha:]]*\\)\\([[:space:],;:!?.()]\\|[*~/]\\|$\\)"
  "Regexp used to expand Duden -suffix abbreviations.")

(defvar sdcv--abbreviation-regexp-cache (make-hash-table :test #'equal)
  "Cached regexps matching dictionary abbreviations by source.")

(defun sdcv--abbreviation-alist (source)
      "Return abbreviation alist from SOURCE.
SOURCE may be an alist, a symbol naming an alist, or a function returning one."
      (cond
       ((symbolp source) (symbol-value source))
       ((functionp source) (funcall source))
       (t source)))

(defun sdcv--abbreviation-regexp (source)
      "Return a regexp that matches abbreviations from SOURCE."
      (or (gethash source sdcv--abbreviation-regexp-cache)
          (puthash
           source
           (concat "\\(?:\\`\\|[^[:alnum:]ÄÖÜäöüß]\\)\\("
                   (regexp-opt
                    (sort (mapcar #'car (sdcv--abbreviation-alist source))
                          (lambda (left right)
                                (> (length left) (length right)))))
                   "\\)\\(?:\\'\\|[^[:alnum:]ÄÖÜäöüß]\\)")
           sdcv--abbreviation-regexp-cache)))

(defun sdcv--abbreviation-lookup-target (meaning)
      "Return the lookup target for abbreviation MEANING."
      (string-trim
       (replace-regexp-in-string
        "[([][^])\n]*[])]" ""
        (car (split-string meaning "[,;]" t "[[:space:]]+")))))

(defun sdcv--highlight-abbreviations (start end source)
      "Color abbreviations from SOURCE and make RET look up their meaning."
      (let ((abbreviations (sdcv--abbreviation-alist source)))
            (save-excursion
                  (goto-char (max (point-min) (1- start)))
                  (while (re-search-forward (sdcv--abbreviation-regexp source) end t)
                        (let* ((abbr (match-string-no-properties 1))
                               (beg (match-beginning 1))
                               (fin (match-end 1))
                               (meaning (cdr (assoc-string abbr abbreviations nil)))
                               (target (and meaning
                                            (save-match-data
                                                  (sdcv--abbreviation-lookup-target meaning)))))
                              (when (and (>= beg start)
                                         meaning
                                         target
                                         (not (string-empty-p target)))
                                    (add-text-properties
                                     beg fin
                                     `(sdcv-lookup-word ,target
                                       sdcv-abbreviation ,abbr
                                       sdcv-abbreviation-meaning ,meaning
                                       help-echo ,(format "%s -> %s" abbr meaning)
                                       mouse-face highlight))
                                    (sdcv--apply-face-overlay
                                     beg fin 'sdcv-abbreviation-link-face 970))
                              (goto-char fin))))))

(defun sdcv-abbreviation-highlighter (source)
      "Return a region post-processor for abbreviation alist SOURCE."
      (lambda (start end _entry _rule)
            (sdcv--highlight-abbreviations start end source)))

(defconst sdcv-duden-abbreviation-highlighter
  (sdcv-abbreviation-highlighter 'sdcv-duden-abbreviations)
  "Post-processor highlighting Duden abbreviations.")

(defconst sdcv-webster-abbreviation-highlighter
  (sdcv-abbreviation-highlighter 'sdcv-webster-abbreviations)
  "Post-processor highlighting Webster abbreviations.")

(defun sdcv--duden-expand-short-form (text)
      "Expand single-letter Duden headword abbreviations inside TEXT."
      (if-let* ((word sdcv--duden-headword)
                                    (initial (and (> (length word) 1)
                                                                              (regexp-quote (substring (downcase word) 0 1)))))
                  (replace-regexp-in-string
                   (format sdcv--duden-short-form-regexp initial)
                   (concat "\\1" word "\\2")
                   text t nil)
            text))

(defun sdcv--duden-expand-quote-shortcuts (text)
      "Expand Duden shorthand forms inside example quote TEXT.
This keeps the expansion Duden-specific by only touching quote content."
      (let ((expanded (sdcv--duden-expand-short-form text)))
            (if-let ((word sdcv--duden-headword))
                    (replace-regexp-in-string
                     sdcv--duden-suffix-form-regexp
                     (lambda (match)
                           (when (string-match
                                          sdcv--duden-suffix-form-regexp
                                          match)
                                 (concat (match-string 1 match)
                                                 word
                                                 (match-string 2 match)
                                                 (match-string 3 match))))
                     expanded t nil)
                  expanded)))

(defun sdcv--duden-match-properties ()
      "Return text properties near the current Duden abbreviation match."
      (let ((positions (delq nil
                             (list (match-end 1)
                                   (match-beginning 2)
                                   (match-beginning 0)))))
            (catch 'properties
                  (dolist (pos positions)
                        (let ((properties (and (<= (point-min) pos)
                                               (< pos (point-max))
                                               (text-properties-at pos))))
                              (when properties
                                    (throw 'properties properties))))
                  nil)))

(defun sdcv--duden-propertize-like-match (text)
      "Return TEXT with the current match's Duden text properties."
      (if-let ((properties (sdcv--duden-match-properties)))
              (let ((copy (copy-sequence text)))
                    (add-text-properties 0 (length copy) properties copy)
                    copy)
            text))

(defun sdcv--duden-expand-suffix-forms (start end entry _rule)
      "Expand Duden -suffix shorthand forms outside bsptext spans between START and END.
Forms like -en or -ist that were not expanded during HTML rendering (i.e. they
appear outside #+begin_quote blocks) are concatenated with the headword here."
      (when-let ((word (alist-get 'word entry)))
            (let ((sdcv--duden-headword word))
                  (save-excursion
                  (goto-char start)
                  (while (re-search-forward
                                        sdcv--duden-suffix-form-regexp
                                        end t)
                              (replace-match
                               (sdcv--duden-propertize-like-match
                                (concat (match-string 1) word
                                        (match-string 2) (match-string 3)))
                               t t))))))

(defun sdcv--duden-highlight-quote-blocks (start end _entry _rule)
      "Apply a background highlight to #+begin_quote … #+end_quote blocks."
      (save-excursion
            (goto-char start)
            (while (re-search-forward "^#\\+begin_quote$" end t)
                  (let* ((block-start (match-beginning 0))
                         (face (if (get-text-property
                                    block-start 'sdcv-duden-idiom-block)
                                       'sdcv-duden-idiom-background-face
                                     'sdcv-duden-quote-background-face)))
                        (when (re-search-forward "^#\\+end_quote$" end t)
                              (sdcv--apply-face-overlay block-start (match-end 0)
                                                        face 800))))))

(defun sdcv--duden-highlight-marked-headwords (start end _entry _rule)
      "Apply one headword overlay to the first Duden headword marker."
      (let ((pos start)
            done)
            (while (and (not done) (< pos end))
                  (let ((next (or (next-single-property-change
                                   pos 'sdcv-duden-headword nil end)
                                  end)))
                        (if (get-text-property pos 'sdcv-duden-headword)
                                (progn
                                      (sdcv--apply-face-overlay pos next
                                                                'sdcv-duden-headword-face
                                                                970)
                                      (setq done t))
                              (setq pos next))))))

(defun sdcv--duden-highlight-references (start end _entry _rule)
      "Underline Duden cross-reference headwords marked with ↑ between START and END."
      (save-excursion
            (goto-char start)
            (while (re-search-forward "↑[[:space:]]*\\([[:alpha:]ÄÖÜäöüß][[:alpha:]ÄÖÜäöüß-]*\\)\\.?" end t)
                  (let ((beg (match-beginning 1))
                        (fin (match-end 1))
                        (target (match-string-no-properties 1)))
                        (add-text-properties beg fin
                                             `(sdcv-lookup-word ,target
                                               mouse-face highlight
                                               help-echo ,(format "Lookup %s" target)))
                        (sdcv--apply-face-overlay beg fin
                                                  'sdcv-duden-reference-face 965)))))

(defun sdcv--duden-polish-section (start end _entry _rule)
      "Normalize rendered Duden output between START and END."
      (let ((content (buffer-substring start end))
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
                                    (string-match-p "\\`[a-z])" line)
                                    (string-match-p "\\`\\* " line)
                                    (string-match-p "\\`\\*[^*\n]+\\*\\(?:[[:space:]]+~<[^>\n]+>~\\)?\\'" line)
                                    (string-match-p "\\`[[:space:]]*[-+] " line))
                        (push line lines))
                   (t
                        (let ((normalized (replace-regexp-in-string "\\`\\*\\*\\* +" "" line)))
                              (push normalized lines)))))
            (save-excursion
                  (delete-region start end)
                  (goto-char start)
                  (insert (mapconcat #'identity (nreverse lines) "\n")))))

(defun sdcv--duden-node-text (node)
      "Return the concatenated plain text content of DOM NODE."
      (sdcv-dom-text node))

(defconst sdcv--duden-superscript-digits
  '((?0 . "⁰") (?1 . "¹") (?2 . "²") (?3 . "³") (?4 . "⁴")
    (?5 . "⁵") (?6 . "⁶") (?7 . "⁷") (?8 . "⁸") (?9 . "⁹"))
  "Mapping from ordinary digits to superscript digits for Duden headwords.")

(defun sdcv--duden-superscript-string (text)
      "Return TEXT with digits rendered as Unicode superscripts."
      (mapconcat
       (lambda (char)
             (or (cdr (assq char sdcv--duden-superscript-digits))
                 (char-to-string char)))
       text
       ""))

(defun sdcv--duden-walk-children (nodes)
      "Render Duden DOM NODES at point."
      (sdcv-dom-each #'sdcv--duden-walk nodes))

(defun sdcv--duden-open-phrase-block ()
      "Open a Duden idiom/proverb quote block unless one is active."
      (unless sdcv--duden-phrase-block-open
            (unless (bolp) (insert "\n"))
            (insert "\n")
            (let ((start (point)))
                  (insert "#+begin_quote\n")
                  (add-text-properties start (point)
                                       '(sdcv-duden-idiom-block t)))
            (setq sdcv--duden-phrase-block-open t)))

(defun sdcv--duden-close-phrase-block ()
      "Close the current Duden idiom/proverb quote block."
      (when sdcv--duden-phrase-block-open
            (unless (bolp) (insert "\n"))
            (insert "#+end_quote\n\n")
            (setq sdcv--duden-phrase-block-open nil)))

(defun sdcv--duden-insert-text (text)
      "Insert Duden TEXT with small spacing fixes."
      (when (and (string-prefix-p "(" text)
                 (not (sdcv--org-pre-boundary-p)))
            (insert " "))
      (insert text))

(defun sdcv--duden-render-nodes-to-string (nodes)
      "Render Duden DOM NODES into a cleaned string."
      (sdcv-rendered-string
       (lambda ()
             (sdcv--duden-walk-children nodes))))

(defun sdcv--duden-walk (node)
      "Walk Duden StarDict DOM NODE, inserting org-oriented text at point."
      (cond
       ((stringp node) (sdcv--duden-insert-text node))
       ((consp node)
            (let* ((tag   (sdcv-dom-tag node))
                               (attrs (sdcv-dom-attrs node))
                               (kids  (sdcv-dom-children node)))
                  (pcase tag
                        ((or 'html 'head 'body) (sdcv--duden-walk-children kids))
                        ('br (insert "\n"))
                        ('sup
                         (insert
                          (sdcv--duden-superscript-string
                           (string-trim
                            (mapconcat #'sdcv--duden-node-text kids "")))))
                        ('b
                         (let ((sub (string-trim (sdcv--duden-render-nodes-to-string kids))))
                               (cond
                                    ((string-empty-p sub) nil)
                                    ((string= sub "*")
                                     (sdcv--duden-open-phrase-block))
                                    ((string= sub "R")
                                     (sdcv--duden-open-phrase-block))
                                    ((string-match-p "\\`[a-z])\\'" sub)
                                     (insert "\n" sub " "))
                                    (t
                                     (when (and sdcv--duden-phrase-block-open
                                                (string-match-p " " sub)
                                                (not (bolp)))
                                           (insert "\n"))
                                     (let ((s (point)))
                                           (insert sub)
                                           (add-text-properties
                                            s (point)
                                           (if (string-match-p " " sub)
                                                    '(face (:weight bold) sdcv-phrase t)
                                                  '(face (:weight bold)
                                                    sdcv-duden-headword t))))))))
                        ('i
                         (let* ((s0 (point))
                                            (_ (sdcv--duden-walk-children kids))
                                            (trimmed (string-trim
                                                                      (buffer-substring-no-properties s0 (point)))))
                               (delete-region s0 (point))
                               (unless (string-empty-p trimmed)
                                     (unless (sdcv--org-pre-boundary-p) (insert " "))
                                     (let ((s (point)))
                                           (insert trimmed)
                                           (put-text-property s (point) 'face '(:slant italic))))))
                        ('u (sdcv--duden-walk-children kids))
                        ('img nil)
                        ('a (sdcv--duden-walk-children kids))
                        ('font
                         (let* ((color (cdr (assq 'color attrs)))
                                                (text  (string-trim
                                                                        (mapconcat #'sdcv--duden-node-text kids ""))))
                               (if (and color
                                                            (member (downcase color)
                                                                                    '("blue" "0000ff" "#0000ff" "3333cc" "#3333cc"))
                                                            (string-match-p "\\`[0-9]+\\.\\'" text))
                                           (progn
                                                 (sdcv--duden-close-phrase-block)
                                                 (insert "\n")
                                                 (insert text)
                                                 (insert " "))
                                     (sdcv--duden-walk-children kids))))
                        ('span
                         (let ((class (cdr (assq 'class attrs)))
                                           (style (or (cdr (assq 'style attrs)) "")))
                               (cond
                                    ((equal class "bsp")
                                     (unless sdcv--duden-phrase-block-open
                                           (insert "\n#+begin_quote\n"))
                                     (when sdcv--duden-phrase-block-open
                                           (unless (bolp) (insert "\n")))
                                     (sdcv--duden-walk-children kids)
                                     (unless (bolp) (insert "\n"))
                                     (unless sdcv--duden-phrase-block-open
                                           (insert "#+end_quote\n")))
                                    ((equal class "bsptext")
                                     (let ((sub (sdcv--duden-render-nodes-to-string kids)))
                                           (unless (string-empty-p sub)
                                                 (insert (sdcv--duden-expand-quote-shortcuts sub))
                                                 (unless (bolp) (insert "\n")))))
                                    ((equal class "meta")
                                     (let ((sub (sdcv--duden-render-nodes-to-string kids)))
                                           (unless (string-empty-p sub)
                                                 (sdcv--insert-org-wrapper "~" sub))))
                                    ((equal class "prag")
                                     (let ((sub (sdcv--duden-render-nodes-to-string kids)))
                                           (unless (string-empty-p sub)
                                                 (unless (sdcv--org-pre-boundary-p)
                                                       (insert " "))
                                                 (insert (string-trim-left sub)))))
                                    ((string-match-p "font-weight[ \t]*:[ \t]*normal" style)
                                     (sdcv--duden-walk-children kids))
                                    (t (sdcv--duden-walk-children kids)))))
                        (_ (sdcv--duden-walk-children kids)))))))

(defun sdcv--duden-insert (html &optional entry _rule)
      "Insert Duden StarDict HTML as formatted text, preserving face properties.
EXTRY provides the headword for expanding Duden short forms."
      (if (not (fboundp 'libxml-parse-html-region))
                  (insert (sdcv--html-strip html entry) "\n")
            (let ((sdcv--duden-headword (alist-get 'word entry))
                  (sdcv--duden-phrase-block-open nil)
                              (target (current-buffer))
                              (tmp (generate-new-buffer " *sdcv-duden-tmp*")))
                  (unwind-protect
                          (progn
                                (with-current-buffer tmp
                                      (insert html)
                                      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
                                            (erase-buffer)
                                            (sdcv--duden-walk dom)
                                            (sdcv--duden-close-phrase-block))
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
                                          (format sdcv--duden-short-form-regexp initial)
                                          end t)
                        (replace-match
                         (sdcv--duden-propertize-like-match
                          (concat (match-string 1) word (match-string 2)))
                         t t)))))

(defun sdcv--duden-highlight-phrases (start end _entry _rule)
      "Highlight text marked as a Duden phrase between START and END."
      (let ((pos start))
            (while (< pos end)
                  (let ((next (or (next-single-property-change pos 'sdcv-phrase nil end)
                                  end)))
                        (when (get-text-property pos 'sdcv-phrase)
                              (sdcv--apply-face-overlay pos next 'sdcv-duden-phrase-face 955))
                        (setq pos next)))))

(defun sdcv--webster-definition-parts (definition)
      "Return (LABEL . BODY) parsed from one Webster DEFINITION."
      (let ((text (string-trim definition)))
            (if (string-match
                 "\\`(<I>\\([^<]+\\)</I>)[[:space:]\n]*<br[[:space:]/]*>[[:space:]\n]*\\(.*\\)\\'"
                 text)
                    (cons (match-string 1 text)
                          (string-trim
                           (replace-regexp-in-string
                            "\\(?:<br[[:space:]/]*>\\|[[:space:]]\\)+\\'"
                            ""
                            (match-string 2 text))))
                  (cons "" text))))

(defun sdcv--webster-group-parts (parts)
      "Group Webster PARTS by grammatical label while preserving order."
      (let (groups)
            (dolist (part parts)
                  (let* ((label (car part))
                         (body (cdr part))
                         (group (assoc label groups #'string=)))
                        (if group
                                (setcdr group (append (cdr group) (list body)))
                              (push (list label body) groups))))
            (nreverse groups)))

(defun sdcv--webster-render-definition-group (group)
      "Render one grouped Webster definition GROUP back to HTML."
      (let ((label (car group))
            (bodies (cdr group)))
            (concat
             (unless (string-empty-p label)
                   (format "(<I>%s</I>) <br>\n" label))
             (mapconcat (lambda (body) (concat body "<br>"))
                        bodies
                        "\n"))))

(defun sdcv--webster-merge-definitions (definitions _entries _rule)
      "Merge Webster DEFINITIONS so repeated labels such as (a.) appear once."
      (mapconcat #'sdcv--webster-render-definition-group
                 (sdcv--webster-group-parts
                  (mapcar #'sdcv--webster-definition-parts definitions))
                 "<br>\n"))

;; Dictionary-specific renderers.
;; Rules can match by exact :name, by :regexp, or by :keywords.  The rule
;; constructors live in sdcv.el; dictionary-specific formatting lives here.
(setq sdcv-rendering-rules
      (sdcv-rendering-rule-list
       (sdcv-html-rendering-rule
        :name "Duden – Deutsches Universalwörterbuch"
        :renderer #'sdcv--duden-insert
        :post-process (sdcv-compose-region-processors
                       #'sdcv--duden-expand-suffix-forms
                       #'sdcv--duden-expand-headword-abbreviations
                       #'sdcv--duden-polish-section
                       #'sdcv--duden-highlight-quote-blocks
                       #'sdcv--duden-highlight-phrases
                       #'sdcv--duden-highlight-references
                       #'sdcv--duden-highlight-marked-headwords
                       sdcv-duden-abbreviation-highlighter)
        :faces '(("\\(~<[^>\n]+>~\\)" 1 sdcv-duden-meta-face)
                 ("^\\([0-9]+\\.\\)" 1 sdcv-duden-sense-face)
                 ("^\\([a-z])\\)" 1 sdcv-duden-sense-face)
                 ;; Abkürzungen aus dem Duden-Abkürzungsverzeichnis -> gold
                 ("\\(?:^\\|[[:space:]/(]\\)\\(jmd\\.\\|jmdm\\.\\|jmdn\\.\\|jmds\\.\\|etw\\.\\|ugs\\.\\|geh\\.\\|bildl\\.\\|übertr\\.\\|iron\\.\\|scherzh\\.\\|verächtl\\.\\|verhüll\\.\\|landsch\\.\\|bes\\.\\|allg\\.\\|dichter\\.\\|hist\\.\\|fachspr\\.\\|selten\\.\\|veraltend\\.\\|schriftl\\.\\|österr\\.\\|schweiz\\.\\|südd\\.\\|nordd\\.\\)" 1 sdcv-abbreviation-face)
                 ("^\\([^#*\n].*\\)$" 1 sdcv-duden-example-face)))
       (sdcv-html-rendering-rule
        :name "Duden – Das Synonymwörterbuch"
        :post-process (sdcv-compose-region-processors
                       #'sdcv--duden-highlight-references
                       sdcv-duden-abbreviation-highlighter)
        :faces '(("\\b[0-9]+\\." . font-lock-constant-face)
                 ("\\b[a-z])" . font-lock-keyword-face)
                 ("\\bSynonyme?\\b" . font-lock-function-name-face)))
       (sdcv-html-rendering-rule
        :name "Duden – Das Fremdwörterbuch"
        :post-process (sdcv-compose-region-processors
                       #'sdcv--duden-highlight-references
                       sdcv-duden-abbreviation-highlighter)
        :faces '(("\\b[0-9]+\\." . font-lock-constant-face)
                 ("\\b[a-z])" . font-lock-keyword-face)
                 ("\\bHerkunft\\b" . font-lock-function-name-face)))
       (sdcv-html-rendering-rule
        :name "Duden - Das Herkunftswörterbuch (De-De)"
        :post-process (sdcv-compose-region-processors
                       #'sdcv--duden-highlight-references
                       sdcv-duden-abbreviation-highlighter)
        :faces '(("\\b[0-9]+\\." . font-lock-constant-face)
                 ("\\b[a-z])" . font-lock-keyword-face)
                 ("\\(?:^\\|[[:space:]/(]\\)\\(Abl\\.\\|Bed\\.\\|Zus\\.\\)"
                  1 sdcv-abbreviation-face)))
       (sdcv-html-rendering-rule
        :name "Webster's Revised Unabridged Dictionary (1913)"
        :merge-definitions #'sdcv--webster-merge-definitions
        :post-process sdcv-webster-abbreviation-highlighter
        :faces '(("\\b\\([0-9]+\\.\\)" 1 font-lock-constant-face)
                 ("\\b\\(n\\.\\|v\\.\\|a\\.\\|adj\\.\\|adv\\.\\)" 1 font-lock-keyword-face)))
       (sdcv-html-rendering-rule
        :keywords '("PONS")
        :faces '(("\\b\\(sb\\.|sth\\.|etw\\.|jdn\\.|jdm\\.|adj\\.|adv\\.|prep\\.|conj\\.\\)\\b"
                  . font-lock-type-face)
                 ("\\bto [^;,]+" . font-lock-function-name-face)))
       (sdcv-html-rendering-rule
        :keywords '("Babylon")
        :faces '(("^\\(n\\.|v\\.|adj\\.|adv\\.\\)" . font-lock-keyword-face)
                 ("(Comput)" . font-lock-type-face)))))

;; ASCII → Org-Entities: für Dictionary-Inhalte wenig sinnvoll (shr rendert
;; HTML-Entities bereits korrekt).  Im sdcv-Buffer mit `e' manuell aufrufbar.
(setq sdcv-auto-org-entities nil)

;; Dictionary HTML carries vendor colors that are often misleading in Emacs
;; themes, so keep semantic faces only.
(setq sdcv-shr-use-colors nil)

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
;; (global-set-key (kbd "C-c M-d") #'sdcv-toggle-dicts)

;;; ── Dictionary toggle UI ─────────────────────────────────────────────────────

(provide 'sdcv-config)
;;; sdcv-config.el ends here
