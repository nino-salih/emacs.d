;;; sdcv-tests.el --- ERT tests for sdcv synonym formatter -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:  M-x ert RET t RET
;; or from the command line:
;;   emacs --batch -Q -L ~/.emacs.d/modules/applications \
;;         -l sdcv -l sdcv-config -l sdcv-tests \
;;         --eval "(ert-run-tests-batch-and-exit)"

;;; Code:

(require 'ert)

(defun sdcv-test--listify (input)
  "Apply `sdcv--duden-synonym-listify' to INPUT string and return result."
  (with-temp-buffer
    (insert input)
    (let ((end (copy-marker (point-max) t)))
      (sdcv--duden-synonym-listify (point-min) end nil nil))
    (buffer-string)))

;;; ── Basic transformations ────────────────────────────────────────────────────

(ert-deftest sdcv-test-listify/letter-prefix-basic ()
  "\"a) w1, w2, w3\" is turned into a letter heading with sorted bullets."
  (should (equal (sdcv-test--listify "a) heißen, sein, darstellen\n")
                 "a)\n  - darstellen\n  - heißen\n  - sein\n")))

(ert-deftest sdcv-test-listify/letter-prefix-with-dot ()
  "\"a.) w1, w2\" (letter + period + paren) is also handled."
  (should (equal (sdcv-test--listify "a.) meinen, ausdrücken\n")
                 "a)\n  - ausdrücken\n  - meinen\n")))

(ert-deftest sdcv-test-listify/plain-commas ()
  "A plain comma-separated line is sorted and turned into bullets."
  (should (equal (sdcv-test--listify "Wien, Berlin, München\n")
                 "  - Berlin\n  - München\n  - Wien\n")))

(ert-deftest sdcv-test-listify/single-item-unchanged ()
  "A line with no comma is left untouched."
  (should (equal (sdcv-test--listify "bedeuten\n")
                 "bedeuten\n")))

(ert-deftest sdcv-test-listify/numbered-line-unchanged ()
  "A bare numbered header like \"1.\" is not touched."
  (should (equal (sdcv-test--listify "1.\n")
                 "1.\n")))

(ert-deftest sdcv-test-listify/hash-line-unchanged ()
  "Lines starting with '#' (org headings) are not touched."
  (should (equal (sdcv-test--listify "# Synonyme\n")
                 "# Synonyme\n")))

;;; ── Sorting ─────────────────────────────────────────────────────────────────

(ert-deftest sdcv-test-listify/already-sorted ()
  "Items that are already in order remain in order."
  (should (equal (sdcv-test--listify "a) alpha, beta, gamma\n")
                 "a)\n  - alpha\n  - beta\n  - gamma\n")))

(ert-deftest sdcv-test-listify/reverse-order-sorted ()
  "Items written in reverse order are sorted ascending."
  (should (equal (sdcv-test--listify "c, b, a\n")
                 "  - a\n  - b\n  - c\n")))

;;; ── Full entry (bedeuten-style) ─────────────────────────────────────────────

(ert-deftest sdcv-test-listify/full-bedeuten-entry ()
  "A complete multi-sense entry is formatted with headings and sorted bullets."
  (let ((input  (concat "1.\n"
                        "a) heißen, sein, darstellen\n"
                        "b) meinen, ausdrücken\n"
                        "2.\n"
                        "a) symbolisieren, verkörpern\n"))
        (expect (concat "1.\n"
                        "a)\n  - darstellen\n  - heißen\n  - sein\n"
                        "b)\n  - ausdrücken\n  - meinen\n"
                        "2.\n"
                        "a)\n  - symbolisieren\n  - verkörpern\n")))
    (should (equal (sdcv-test--listify input) expect))))

;;; ── Semicolons as additional separators ─────────────────────────────────────

(ert-deftest sdcv-test-listify/semicolon-separator ()
  "Semicolons are treated as separators alongside commas."
  (should (equal (sdcv-test--listify "a) alpha; beta, gamma\n")
                 "a)\n  - alpha\n  - beta\n  - gamma\n")))

;;; ── Two-item minimum ─────────────────────────────────────────────────────────

(ert-deftest sdcv-test-listify/letter-prefix-one-item-unchanged ()
  "A letter-prefix line with only one item is not reformatted."
  (should (equal (sdcv-test--listify "a) einzig\n")
                 "a) einzig\n")))

(provide 'sdcv-tests)
;;; sdcv-tests.el ends here
