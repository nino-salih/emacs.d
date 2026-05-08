;;; -*- lexical-binding: t; -*-

;; If you install it manually, set for example:
;; (setq lsp-clojure-custom-server-command '("bash" "-c" "/path/to/clojure-lsp"))

(defvar my/clojure-lsp-installing nil
  "Non-nil while clojure-lsp is being installed by lsp-mode.")

(defun my/clojure-lsp-installed-p ()
  "Return non-nil when clojure-lsp is available locally."
  (require 'lsp-mode)
  (require 'lsp-clojure nil t)
  (or (executable-find "clojure-lsp")
      (and (fboundp 'lsp-package-path)
           (ignore-errors
             (let ((path (lsp-package-path 'clojure-lsp)))
               (and path (file-executable-p path)))))))

(defun my/clojure-ensure-lsp ()
  "Ensure clojure-lsp is installed, then start lsp-mode."
  (interactive)
  (require 'lsp-mode)
  (require 'lsp-clojure nil t)
  (if (my/clojure-lsp-installed-p)
      (lsp)
    (if my/clojure-lsp-installing
        (message "Clojure: clojure-lsp installation already running...")
      (setq my/clojure-lsp-installing t)
      (message "Clojure: installing clojure-lsp via lsp-mode...")
      (let ((source-buffer (current-buffer)))
        (lsp-package-ensure
         'clojure-lsp
         (lambda ()
           (setq my/clojure-lsp-installing nil)
           (message "Clojure: clojure-lsp installed successfully.")
           (when (buffer-live-p source-buffer)
             (with-current-buffer source-buffer
               (lsp))))
         (lambda (error-message)
           (setq my/clojure-lsp-installing nil)
           (display-warning 'clojure
                            (format "clojure-lsp installation failed: %s"
                                    error-message)
                            :warning)))))))

(use-package clojure-mode
  :straight t
  :ensure-system-package lein
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  :hook
  (clojure-mode . my/clojure-ensure-lsp)
  (clojurescript-mode . my/clojure-ensure-lsp)
  (clojurec-mode . my/clojure-ensure-lsp))

(use-package cider
  :straight t
  :after clojure-mode
  :bind (:map cider-mode-map
         ([remap my/lsp-describe-and-focus] . cider-clojuredocs))
  :hook
  ;; CIDER's capf provides documentation for corfu-popupinfo; put it first so
  ;; LSP's capf (which often lacks docs) does not win the race.
  (cider-mode . (lambda ()
                  (setq-local completion-at-point-functions
                              (cons #'cider-complete-at-point
                                    (remq #'cider-complete-at-point
                                          completion-at-point-functions)))))
  ;; Use CIDER eldoc (symbol-at-point) instead of LSP hover (outer form).
  ;; LSP would show `apply' when cursor is on `concat' in (apply concat …).
  (cider-mode . (lambda ()
                  (setq-local lsp-eldoc-enable-hover nil)))
  :custom
  ;; Show eldoc for the symbol under point, not the enclosing call.
  (cider-eldoc-display-for-symbol-at-point t))
