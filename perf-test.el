;;; perf-test.el --- Benchmark rainbow-delimiters scenarios  -*- lexical-binding: t; -*-
;;
;; Run with:
;;   emacs --batch -l perf-test.el
;;
;; Scenarios:
;;   1. baseline  – no rainbow-delimiters at all
;;   2. standard  – rainbow-delimiters with default pick-face function
;;   3. custom    – rainbow-delimiters with my/rainbow-pick-face-per-type (tree-sitter)

;; --------------------------------------------------------------------------
;; Setup paths
;; --------------------------------------------------------------------------
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

;; Use straight/build dirs which contain compiled .elc files
(let ((build-dir (expand-file-name "straight/build" user-emacs-directory)))
  (dolist (pkg '("rust-mode" "rainbow-delimiters" "dash"))
    (add-to-list 'load-path (expand-file-name pkg build-dir))))

;; Ensure the built-in treesit module is loaded
(require 'treesit nil t)

;; Add tree-sitter grammar libraries if present
(let ((ts-lib-dir (expand-file-name "tree-sitter" user-emacs-directory)))
  (when (file-directory-p ts-lib-dir)
    (add-to-list 'treesit-extra-load-path ts-lib-dir)))

(defvar perf/target-file "/home/nino/Software/partydeck/src/large_test.rs")
(defvar perf/iterations 5)

;; --------------------------------------------------------------------------
;; Helpers
;; --------------------------------------------------------------------------
(defun perf/load-rainbow ()
  "Load the rainbow-delimiters library (no activation)."
  (require 'rainbow-delimiters))

(defun perf/activate-font-lock (buf)
  "Force a full font-lock re-fontification on BUF and wait for it."
  (with-current-buffer buf
    (font-lock-mode 1)
    (font-lock-ensure (point-min) (point-max))))

(defun perf/time-open-and-fontify (setup-fn &optional teardown-fn)
  "Time ITERATIONS runs of: open file fresh, run SETUP-FN, fontify, kill.
Returns a plist with :min :max :mean in seconds."
  (let (times)
    (dotimes (_ perf/iterations)
      (let* ((buf (find-file-noselect perf/target-file t))  ; t = nowarn, raw
             (start (float-time)))
        (with-current-buffer buf
          (funcall setup-fn))
        (perf/activate-font-lock buf)
        (push (- (float-time) start) times)
        (when teardown-fn (with-current-buffer buf (funcall teardown-fn)))
        (kill-buffer buf)))
    (let* ((sorted (sort times #'<))
           (n      (length sorted))
           (sum    (apply #'+ sorted)))
      (list :min  (car sorted)
            :max  (car (last sorted))
            :mean (/ sum n)
            :all  sorted))))

;; --------------------------------------------------------------------------
;; Scenario setup functions
;; --------------------------------------------------------------------------

;; --- Scenario 1: baseline (no rainbow) ---
(defun perf/setup-baseline ()
  (rust-mode))      ; just activate the major mode, no rainbow

;; --- Scenario 2: standard rainbow (default pick-face) ---
(defun perf/setup-standard ()
  (rust-mode)
  (rainbow-delimiters-mode 1))

;; --- Scenario 3: custom pick-face (our tree-sitter aware version) ---
(defun perf/setup-custom ()
  (rust-mode)
  ;; Install our custom pick-face function
  (setq-local rainbow-delimiters-pick-face-function #'my/rainbow-pick-face-per-type)
  ;; Ensure tree-sitter parser is active
  (my/rainbow-ensure-treesit-parser)
  (rainbow-delimiters-mode 1))

;; --------------------------------------------------------------------------
;; Load dependencies
;; --------------------------------------------------------------------------
(message "\n=== Loading dependencies ===")

;; Load rust-mode (use compiled build dir)
(condition-case err
    (require 'rust-mode)
  (error (message "rust-mode load error: %s" err)))

;; Load rainbow-delimiters
(condition-case err
    (perf/load-rainbow)
  (error (message "rainbow-delimiters load error: %s" err)))

;; Load our custom functions from the module file
(load (expand-file-name "modules/programming/rainbow-delimiters.el"
                        user-emacs-directory)
      nil 'nomessage)

;; --------------------------------------------------------------------------
;; Run benchmarks
;; --------------------------------------------------------------------------
(message "\n=== Performance Benchmark: rainbow-delimiters ===")
(message "File : %s" perf/target-file)
(message "Runs : %d" perf/iterations)
(message "")

(defun perf/report (label result)
  (message "%-12s  min=%5.1fms  mean=%5.1fms  max=%5.1fms"
           label
           (* 1000 (plist-get result :min))
           (* 1000 (plist-get result :mean))
           (* 1000 (plist-get result :max))))

;; Scenario 1 — baseline
(message "Running: baseline ...")
(let ((r (condition-case err
             (perf/time-open-and-fontify #'perf/setup-baseline)
           (error (message "ERROR baseline: %s" err) nil))))
  (when r (perf/report "baseline" r)))

;; Scenario 2 — standard rainbow
(message "Running: standard rainbow ...")
(let ((r (condition-case err
             (perf/time-open-and-fontify #'perf/setup-standard)
           (error (message "ERROR standard: %s" err) nil))))
  (when r (perf/report "standard" r)))

;; Scenario 3 — custom (tree-sitter depth)
(message "Running: custom (tree-sitter) ...")
(let ((r (condition-case err
             (perf/time-open-and-fontify #'perf/setup-custom)
           (error (message "ERROR custom: %s" err) nil))))
  (when r (perf/report "custom" r)))

(message "\n=== Done ===\n")
