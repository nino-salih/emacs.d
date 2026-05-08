;;; sdcv-tests.el --- ERT tests for sdcv rendering config -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q -L ~/.emacs.d/modules/applications \
;;         -l sdcv -l sdcv-config -l sdcv-tests \
;;         --eval "(ert-run-tests-batch-and-exit)"

;;; Code:

(require 'ert)
(require 'sdcv)
(require 'sdcv-config)

(defconst sdcv-test--duden-html
  (concat
   "<FONT color=\"blue\"><B>l<U>au</U>fen</B></FONT>"
   "<SPAN class=\"meta\"> &lt;st. V.&gt; </SPAN>"
   "<BR><BR><FONT color=\"blue\">1.</FONT><BR>"
   "<B>a)</B><I>sich fortbewegen: </I>"
   "<SPAN class=\"bsp\"><SPAN class=\"bsptext\">"
   "er musste l., um den Bus zu bekommen; "
   "</SPAN></SPAN>"))

(defun sdcv-test--entry (dict word &optional definition)
  "Build a minimal sdcv result entry for tests."
  `((dict . ,dict)
    (word . ,word)
    (definition . ,definition)))

(defun sdcv-test--count-matches (regexp text)
  "Return number of REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(ert-deftest sdcv-test-duden-universal-does-not-create-org-lists ()
  "Duden Universal rendering should not invent org bullet lists."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let* ((entry (sdcv-test--entry "Duden – Deutsches Universalwörterbuch"
                                  "laufen"
                                  sdcv-test--duden-html))
         (output (with-temp-buffer
                   (sdcv--render-entry "laufen" entry)
                   (buffer-string))))
    (should (string-match-p "^a) sich fortbewegen:" output))
    (should (string-match-p "^er musste laufen, um den Bus zu bekommen;" output))
    (should-not (string-match-p "^[[:space:]]*-[[:space:]]+a)" output))
    (should-not (string-match-p "^[[:space:]]*-[[:space:]]+er musste" output))))

(ert-deftest sdcv-test-synonym-rule-highlights-abbreviations ()
  "Synonym dictionaries should highlight abbreviations without listification."
  (let ((rule (sdcv--rendering-rule "Duden – Das Synonymwörterbuch")))
    (should (functionp (plist-get rule :post-process)))
    (should (eq (plist-get rule :renderer) #'sdcv--html-insert))))

(ert-deftest sdcv-test-fremdwort-rule-highlights-abbreviations ()
  "Fremdwort dictionaries should highlight abbreviations too."
  (let ((rule (sdcv--rendering-rule "Duden – Das Fremdwörterbuch")))
    (should (functionp (plist-get rule :post-process)))))

(ert-deftest sdcv-test-webster-rule-has-own-abbreviations ()
  "Webster should use its own abbreviation alist."
  (let ((rule (sdcv--rendering-rule
               "Webster's Revised Unabridged Dictionary (1913)")))
    (should (eq (plist-get rule :post-process)
                sdcv-webster-abbreviation-highlighter))
    (should (assoc "Zoöl." sdcv-webster-abbreviations))
    (should-not (assoc "Zoöl." sdcv-duden-abbreviations))))

(ert-deftest sdcv-test-webster-is-preferred-dict ()
  "Webster should be part of the configured preferred dictionaries."
  (should (member "Webster's Revised Unabridged Dictionary (1913)"
                  (mapcar (lambda (dict) (plist-get dict :name))
                          sdcv-preferred-dicts))))

(ert-deftest sdcv-test-render-merges-same-dict-entries-under-one-header ()
  "Multiple entries from one dictionary should share one dictionary heading."
  (let* ((dict "Webster's Revised Unabridged Dictionary (1913)")
         (entries (list (sdcv-test--entry dict "test" "First definition.")
                        (sdcv-test--entry dict "test" "Second definition.")))
         (output (with-temp-buffer
                   (sdcv--render "test" entries 'exact)
                   (buffer-string))))
    (should (= 1 (sdcv-test--count-matches "^\\* Webster$" output)))
    (should (string-match-p "First[[:space:]]+definition\\." output))
    (should (string-match-p "Second[[:space:]]+definition\\." output))))

(ert-deftest sdcv-test-render-keeps-different-duden-dicts-separated ()
  "Rendering adjacent Duden dictionaries must not move one body under another header."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let* ((entries (list (sdcv-test--entry "Duden – Deutsches Universalwörterbuch"
                                          "sehr"
                                          sdcv-test--duden-html)
                        (sdcv-test--entry "Duden – Das Synonymwörterbuch"
                                          "sehr"
                                          "<font color=0000FF>sehr</font><br>ausgesprochen, äußerst.")))
         (output (with-temp-buffer
                   (sdcv--render "sehr" entries 'exact)
                   (buffer-string)))
         (duden-heading (string-match "^\\* Duden DE$" output))
         (duden-body (string-match "^laufen" output))
         (synonym-heading (string-match "^\\* Duden Synonym$" output))
         (synonym-body (string-match "ausgesprochen" output synonym-heading)))
    (should duden-heading)
    (should duden-body)
    (should synonym-heading)
    (should synonym-body)
    (should (< duden-heading duden-body))
    (should (< duden-body synonym-heading))
    (should (< synonym-heading synonym-body))))

(ert-deftest sdcv-test-duden-headword-uses-overlay-not-org-bold-markers ()
  "Duden Universal headwords should be styled without visible org bold markers."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((entry (sdcv-test--entry "Duden – Deutsches Universalwörterbuch"
                                 "laufen"
                                 sdcv-test--duden-html)))
    (with-temp-buffer
      (sdcv--render-entry "laufen" entry)
      (should-not (string-match-p "\\*laufen\\*" (buffer-string)))
      (goto-char (point-min))
      (should (re-search-forward "laufen" nil t))
      (should (seq-some (lambda (overlay)
                          (eq (overlay-get overlay 'face)
                              'sdcv-duden-headword-face))
                        (overlays-at (match-beginning 0))))
      (should (re-search-forward "er musste laufen" nil t))
      (should-not (seq-some (lambda (overlay)
                              (eq (overlay-get overlay 'face)
                                  'sdcv-duden-headword-face))
                            (overlays-at (1- (match-end 0))))))))

(ert-deftest sdcv-test-duden-marked-headword-gets-overlay-when-text-differs-from-query ()
  "Duden headword overlays should not depend on exact lookup-word spelling."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((entry (sdcv-test--entry
                "Duden – Deutsches Universalwörterbuch"
                "exzentrisch"
                "<FONT color=\"blue\"><B>ex<FONT color=\"black\">|</FONT>zentrisch</B></FONT>")))
    (with-temp-buffer
      (sdcv--render-entry "exzentrisch" entry)
      (goto-char (point-min))
      (should (re-search-forward (regexp-quote "ex|zentrisch") nil t))
      (should (seq-some (lambda (overlay)
                          (eq (overlay-get overlay 'face)
                              'sdcv-duden-headword-face))
                        (overlays-at (match-beginning 0)))))))

(ert-deftest sdcv-test-duden-headword-superscript-numbers-stay-superscript ()
  "Duden homograph numbers should not collapse into the headword text."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((entry (sdcv-test--entry
                "Duden – Deutsches Universalwörterbuch"
                "ein"
                "<FONT color=\"blue\"><B><SUP>1</SUP><U>ei</U>n</B></FONT>")))
    (with-temp-buffer
      (sdcv--render-entry "ein" entry)
      (should (string-match-p "¹ein" (buffer-string)))
      (should-not (string-match-p "\\b1ein\\b" (buffer-string))))))

(ert-deftest sdcv-test-duden-expanded-phrases-keep-overlay-from-first-word ()
  "Duden shorthand expansion should preserve phrase markers."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((entry (sdcv-test--entry
                "Duden – Deutsches Universalwörterbuch"
                "ein"
                "<B><B>e. für alle Mal </B></B><BR><B><B>in -em fort</B></B>")))
    (with-temp-buffer
      (sdcv--render-entry "ein" entry)
      (goto-char (point-min))
      (should (re-search-forward "ein für alle Mal" nil t))
      (should (seq-some (lambda (overlay)
                          (eq (overlay-get overlay 'face)
                              'sdcv-duden-phrase-face))
                        (overlays-at (match-beginning 0))))
      (should (re-search-forward "in einem fort" nil t))
      (should (seq-some (lambda (overlay)
                          (eq (overlay-get overlay 'face)
                              'sdcv-duden-phrase-face))
                        (overlays-at (+ (match-beginning 0) 3)))))))

(ert-deftest sdcv-test-duden-idiom-blocks-include-proverbs-and-expand-shortcuts ()
  "Duden idiom blocks should include R/* phrases and expand punctuation cases."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let* ((html (concat
                "<FONT color=\"blue\"><B>Bein</B></FONT>"
                "<BR><BR><FONT color=\"blue\">1.</FONT>"
                "<SPAN class=\"bsp\"><SPAN class=\"bsptext\">"
                "sich mühsam auf die -e stellen; "
                "</SPAN></SPAN>"
                "<BR><BR><B>R</B>auf einem B. kann man nicht stehen; "
                "<BR><BR><B>*</B><B><B>kein B. </B></B>"
                "<SPAN class=\"prag\">(landsch.; </SPAN><I>kein Mensch); </I>"
                "<B><B>jmdn., sich [wieder] auf die -e bringen/stellen </B></B>"
                "(<I>jmdn., sich [wieder] aufrichten: </I>"
                "<SPAN class=\"bsp\"><SPAN class=\"bsptext\">"
                "ich stellte mich mühsam auf die -e. "
                "</SPAN></SPAN><I>wieder aufrichten); </I>"
                "<BR><BR><FONT color=\"blue\">2.</FONT><I>Möbelteil.</I>"))
         (entry (sdcv-test--entry "Duden – Deutsches Universalwörterbuch"
                                  "Bein"
                                  html)))
    (with-temp-buffer
      (sdcv--render-entry "Bein" entry)
      (let* ((output (buffer-string))
             (proverb (string-match "auf einem Bein kann man nicht stehen" output))
             (idiom (string-match "kein Bein" output))
             (next-idiom (string-match "jmdn., sich \\[wieder\\] auf die Beine bringen/stellen" output))
             (example (string-match "ich stellte mich mühsam auf die Beine\\." output))
             (next-sense (string-match "^2\\." output))
             (first-block (string-match "#\\+begin_quote" output))
             (block-start (and proverb
                               (string-match "#\\+begin_quote" output
                                             (string-match "#\\+end_quote" output))))
             (block-end (and block-start
                             (string-match "#\\+end_quote" output block-start))))
        (should proverb)
        (should idiom)
        (should next-idiom)
        (should example)
        (should next-sense)
        (should first-block)
        (should block-start)
        (should block-end)
        (should (seq-some (lambda (overlay)
                            (eq (overlay-get overlay 'face)
                                'sdcv-duden-quote-background-face))
                          (overlays-at (1+ first-block))))
        (should (seq-some (lambda (overlay)
                            (eq (overlay-get overlay 'face)
                                'sdcv-duden-idiom-background-face))
                          (overlays-at (1+ block-start))))
        (should (< block-start proverb))
        (should (< proverb idiom))
        (should (< idiom next-idiom))
        (should (< next-idiom example))
        (should (< example block-end))
        (should (< block-end next-sense))
        (should (string-match-p "^kein Bein " output))
        (should (string-match-p
                 "^jmdn., sich \\[wieder\\] auf die Beine bringen/stellen "
                 output))
        (should-not (string-match-p "-e\\." output))
        (should-not (string-match-p "\\bB\\.(" output))))))

(ert-deftest sdcv-test-merge-groups-require-exact-dict-name ()
  "Same-word entries from different dictionaries must not be merge candidates."
  (let* ((left (sdcv-test--entry "Dict A" "gleich" "A"))
         (right (sdcv-test--entry "Dict B" "gleich" "B"))
         (groups (sdcv--group-entries-by-word (list left right))))
    (should (= 2 (length groups)))
    (should (equal (mapcar #'car groups)
                   '(("Dict A" "gleich") ("Dict B" "gleich"))))))

(ert-deftest sdcv-test-render-uses-overlay-title-instead-of-org-title ()
  "The lookup title should be plain text styled by overlay, not #+TITLE."
  (with-temp-buffer
    (sdcv--render "laufen" nil 'empty)
    (should (string-prefix-p "laufen\n\n" (buffer-string)))
    (should-not (string-match-p "#\\+TITLE:" (buffer-string)))
    (goto-char (point-min))
    (should (seq-some (lambda (overlay)
                        (eq (overlay-get overlay 'face) 'sdcv-title-face))
                      (overlays-at (point))))))

(ert-deftest sdcv-test-duden-references-are-underlined-and-linked ()
  "Duden ↑ references should underline the target word and make it lookupable."
  (with-temp-buffer
    (insert "Siehe ↑exzentrisch.\n")
    (sdcv--duden-highlight-references (point-min) (point-max) nil nil)
    (goto-char (point-min))
    (should (re-search-forward "exzentrisch" nil t))
    (should (equal (get-text-property (match-beginning 0) 'sdcv-lookup-word)
                   "exzentrisch"))
    (should (seq-some (lambda (overlay)
                        (eq (overlay-get overlay 'face)
                            'sdcv-duden-reference-face))
                      (overlays-at (match-beginning 0))))))

(ert-deftest sdcv-test-webster-merges-repeated-grammatical-labels ()
  "Webster same-word matches should group repeated labels such as (a.) and (n.)."
  (let* ((dict "Webster's Revised Unabridged Dictionary (1913)")
         (entries (list (sdcv-test--entry dict "Ancient"
                                          "\n(<I>a.</I>) <br>\nDignified.<br>")
                        (sdcv-test--entry dict "Ancient"
                                          "\n(<I>a.</I>) <br>\nExperienced.<br>")
                        (sdcv-test--entry dict "Ancient"
                                          "\n(<I>n.</I>) <br>\nA senior.<br>")
                        (sdcv-test--entry dict "Ancient"
                                          "\n(<I>n.</I>) <br>\nAn aged man.<br>")))
         (output (with-temp-buffer
                   (sdcv--render "Ancient" entries 'exact)
                   (buffer-string))))
    (should (= 1 (sdcv-test--count-matches "^(a\\.)$" output)))
    (should (= 1 (sdcv-test--count-matches "^(n\\.)$" output)))
    (should (string-match-p "Dignified\\." output))
    (should (string-match-p "Experienced\\." output))
    (should (string-match-p "A[[:space:]]+senior\\." output))
    (should (string-match-p "An[[:space:]]+aged[[:space:]]+man\\." output))))

(ert-deftest sdcv-test-vendor-html-colors-disabled ()
  "Configuration should ignore dictionary-provided HTML colors."
  (should-not sdcv-shr-use-colors))

(ert-deftest sdcv-test-abbreviations-are-linked-to-meanings ()
  "Known dictionary abbreviations should be blue links to their meanings."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((entry (sdcv-test--entry "Duden – Deutsches Universalwörterbuch"
                                 "laufen"
                                 sdcv-test--duden-html)))
    (with-temp-buffer
      (sdcv--render-entry "laufen" entry)
      (goto-char (point-min))
      (should (re-search-forward "st\\. V\\." nil t))
      (should (equal (get-text-property (match-beginning 0) 'sdcv-lookup-word)
                     "starkes Verb"))
      (should (equal (get-text-property (match-beginning 0)
                                        'sdcv-abbreviation-meaning)
                     "starkes Verb"))
      (let* ((overlays (overlays-at (match-beginning 0)))
             (faces (mapcar (lambda (overlay) (overlay-get overlay 'face))
                            overlays)))
        (should (memq 'sdcv-abbreviation-link-face faces))))))

(ert-deftest sdcv-test-abbreviation-doc-at-point ()
  "Abbreviations should expose their meaning at point."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((entry (sdcv-test--entry "Duden – Deutsches Universalwörterbuch"
                                 "laufen"
                                 sdcv-test--duden-html)))
    (with-temp-buffer
      (sdcv--render-entry "laufen" entry)
      (goto-char (point-min))
      (should (re-search-forward "st\\. V\\." nil t))
      (goto-char (match-beginning 0))
      (should (equal (sdcv--abbreviation-doc-at-point)
                     "st. V. -> starkes Verb")))))

(ert-deftest sdcv-test-abbreviation-doc-can-be-disabled ()
  "The abbreviation display should respect `sdcv-show-abbreviation-doc'."
  (with-temp-buffer
    (insert "ugs.")
    (add-text-properties (point-min) (point-max)
                         '(sdcv-abbreviation "ugs."
                           sdcv-abbreviation-meaning "umgangssprachlich"))
    (goto-char (point-min))
    (let ((sdcv-show-abbreviation-doc nil))
      (should-not (sdcv--abbreviation-doc-at-point)))))

(ert-deftest sdcv-test-sdcv-mode-installs-abbreviation-overlay-updater ()
  "sdcv-mode should add its abbreviation overlay updater locally."
  (with-temp-buffer
    (let ((sdcv-show-abbreviation-doc t))
      (sdcv-mode 1)
      (should (memq #'sdcv--update-abbreviation-doc-overlay
                    post-command-hook)))))

(ert-deftest sdcv-test-abbreviation-doc-overlay-is-inline ()
  "Abbreviation docs should be shown as an overlay in the sdcv buffer."
  (with-temp-buffer
    (insert "ugs.")
    (add-text-properties (point-min) (point-max)
                         '(sdcv-abbreviation "ugs."
                           sdcv-abbreviation-meaning "umgangssprachlich"))
    (goto-char (point-min))
    (let ((sdcv-show-abbreviation-doc t))
      (sdcv-mode 1)
      (sdcv--update-abbreviation-doc-overlay)
      (should (overlayp sdcv--abbreviation-doc-overlay))
      (should (string-match-p "ugs\\. -> umgangssprachlich"
                              (overlay-get sdcv--abbreviation-doc-overlay
                                           'after-string))))))

(ert-deftest sdcv-test-abbreviation-aliases-are-linked ()
  "Comma-separated abbreviation aliases should resolve individually."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((entry (sdcv-test--entry "Duden – Das Fremdwörterbuch"
                                 "Test"
                                 "<p>franz. Herkunft</p>")))
    (with-temp-buffer
      (sdcv--render-entry "Test" entry)
      (goto-char (point-min))
      (should (re-search-forward "franz\\." nil t))
      (should (equal (get-text-property (match-beginning 0) 'sdcv-lookup-word)
                     "französisch")))))

(ert-deftest sdcv-test-webster-abbreviations-are-linked ()
  "Webster abbreviations should resolve through the Webster alist only."
  (let ((entry (sdcv-test--entry
                "Webster's Revised Unabridged Dictionary (1913)"
                "Test"
                "<p>Zoöl. usage</p>")))
    (with-temp-buffer
      (sdcv--render-entry "Test" entry)
      (goto-char (point-min))
      (should (re-search-forward "Zoöl\\." nil t))
      (should (equal (get-text-property (match-beginning 0) 'sdcv-lookup-word)
                     "Zoölogy")))))

(ert-deftest sdcv-test-lookup-target-prefers-text-property ()
  "Lookup at point should prefer rendered cross-reference properties."
  (with-temp-buffer
    (insert "ugs.")
    (add-text-properties (point-min) (point-max)
                         '(sdcv-lookup-word "umgangssprachlich"))
    (goto-char (point-min))
    (should (equal (sdcv--lookup-target-at-point) "umgangssprachlich"))))

(provide 'sdcv-tests)
;;; sdcv-tests.el ends here
