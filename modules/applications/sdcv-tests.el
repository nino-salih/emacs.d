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

(ert-deftest sdcv-test-synonym-rule-has-no-list-postprocessor ()
  "Synonym dictionaries should use plain HTML rendering without listification."
  (let ((rule (sdcv--rendering-rule "Duden – Das Synonymwörterbuch")))
    (should-not (plist-get rule :post-process))
    (should (eq (plist-get rule :renderer) #'sdcv--html-insert))))

(ert-deftest sdcv-test-vendor-html-colors-disabled ()
  "Configuration should ignore dictionary-provided HTML colors."
  (should-not sdcv-shr-use-colors))

(provide 'sdcv-tests)
;;; sdcv-tests.el ends here
