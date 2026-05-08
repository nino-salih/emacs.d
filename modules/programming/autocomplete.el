;;; -*- lexical-binding: t; -*-
;; A few more useful configurations...
(use-package emacs
  :init

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))


;; https://archive.is/Gj6Fu
;; Adds File Path Completion
(autoload 'ffap-file-at-point "ffap")
(defun complete-path-at-point+ ()
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))
(add-hook 'completion-at-point-functions
          #'complete-path-at-point+
          'append)

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;
;; Taken from the Vertico docs.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Use consult-completion-in-region as fallback when corfu is not active
;; (e.g. in special buffers). Corfu sets its own completion-in-region-function
;; via global-corfu-mode and must not be overridden here.
(setq-default completion-in-region-function
              (lambda (&rest args)
                (apply (if (and vertico-mode (not (bound-and-true-p corfu-mode)))
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args)))