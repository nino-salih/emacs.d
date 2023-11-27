;;; -*- lexical-binding: t; -*-
;; Some snake oil during the startup.  Probably won't hurt and is
;; reverted right after the Emacs initialization finishes.
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold ,gc-cons-threshold))
          'append)
(setq gc-cons-threshold most-positive-fixnum)


;; prevent package.el from loading packages at startup
(setq package-enable-at-startup nil)


;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(defun bootstrap-use-package ()
  "Install use-package.el"
  (setq use-package-enable-imenu-support t)
  (straight-use-package 'use-package)
  (straight-use-package 'diminish))

;; Load the machine-local config.
(unless (getenv "EMACS_NO_LOCAL")
  (let ((local-lisp (expand-file-name "early-local" user-emacs-directory)))
    (load local-lisp 'noerror)))


;; Disable the GUI elements I don't use.
(dolist (mode '(scroll-bar-mode
                horizontal-scroll-bar-mode
                menu-bar-mode
                tool-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))


(bootstrap-use-package)
(setq inhibit-startup-message t) ;; Don't show the startup screen


;; do we have native compilation available?
(defconst have-native-compilation
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)))

(when have-native-compilation
  ;; don't blast me with (mostly) useless warnings
  (setq native-comp-async-report-warnings-errors nil)
  ;; keep the eln cache tidy
  (setq native-comp-eln-load-path
        (list (concat user-emacs-directory "eln-cache/")
              (car (last native-comp-eln-load-path))))
  ;; needed for emacs-29.1
  (setq native-comp-deferred-compilation-deny-list nil))