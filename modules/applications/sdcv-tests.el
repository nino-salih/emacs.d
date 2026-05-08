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
    (should (eq (plist-get rule :post-process)
                sdcv-duden-abbreviation-highlighter))
    (should (eq (plist-get rule :renderer) #'sdcv--html-insert))))

(ert-deftest sdcv-test-fremdwort-rule-highlights-abbreviations ()
  "Fremdwort dictionaries should highlight abbreviations too."
  (let ((rule (sdcv--rendering-rule "Duden – Das Fremdwörterbuch")))
    (should (eq (plist-get rule :post-process)
                sdcv-duden-abbreviation-highlighter))))

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
