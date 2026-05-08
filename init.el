;;; -*- lexical-binding: t; -*-
;;; Code:
;; Load no-littering.el before anything else to keep ~/.emacs.d/ tidy.
(use-package no-littering :straight t)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; used for Modern LSP Server default is 64KB now we have 4MB
(setq read-process-output-max (* 4 1024 1024))

(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(defun load-directory (dir depth)
  "Load all Lisp files in a DIR to a specified depth."
  (let ((files (directory-files dir t nil)))
    (dolist (file files)
      (if (and (file-directory-p file) (not (string-prefix-p "." (file-name-nondirectory file))))
          (if (> depth 0)
              (load-directory file (1- depth)))
      (when (string-suffix-p ".el" file)
        (load file))))))

(defun load-lisp-files-in-dir (subdir depth)
  "Load all Lisp files in a directory within `user-emacs-directory` and its subdirectories up to a specified depth."
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (when (file-directory-p dir)
      (load-directory dir depth))))

;; Helpfully for debugging
(defun eval-and-close-file ()
  "Evaluate the file at the specified path and then close it."
  (interactive)
  (let ((file-path (read-file-name "Enter the file path: ")))
    (with-temp-buffer
      (insert-file-contents file-path)
      (eval-buffer))
    (message "File evaluated")))

(defun my/reload-config ()
  "Reload the Emacs configuration from `user-init-file`."
  (interactive)
  (load-file user-init-file)
  (message "Emacs configuration reloaded"))


;; disable redisplay parenthesis algorithm for right-to-left (Arabic, Hebrew etc)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; disable syntax-highlight while typing
(setq redisplay-skip-fontification-on-input t)

;; Save Clipboard before Killing
(setq save-interprogram-paste-before-kill t)

;; Disable Dupblicates in Kill Ring
(setq kill-do-not-save-duplicates t)

;; Presist Kill Ring after Restarts
(savehist-mode 1)

(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))

;; remove fonts, overlays etc so the savehist will not bloat
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))

;; Enable recent files
(recentf-mode 1)

;; make a script automatically executable after save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Remove the double escape everything, default is 'read
(setq reb-re-syntax 'string)

(load-lisp-files-in-dir "modules" 5)

(provide 'init)
;;; init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
