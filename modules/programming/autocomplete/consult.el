;;; -*- lexical-binding: t; -*-
;; Consult is a library for narrowing and searching.
(use-package consult
  :straight t
  :bind (:map consult-mode-map
         ;; M-s …
         ("M-s u" . consult-focus-lines)
         ("M-s k" . consult-keep-lines)
         ("M-s e" . consult-isearch-history)
         ("M-s d" . consult-find)
         ;; M-g …
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
	     ("C-c a i" . consult-imenu)
	     ("M-g i" . consult-imenu)
         ("M-g I" . consult-info) 
         ("M-g r" . consult-ripgrep)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ;; Misc.
         ("C-x C-r" . consult-recent-file)
         ;; Remaps
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap project-switch-to-buffer]      . consult-project-buffer)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap goto-line]                     . consult-goto-line)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap repeat-complex-command]        . consult-complex-command)
         ;; Remaps for `Info-mode'.
         ([remap Info-search] . consult-info)

         :map isearch-mode-map
         ("TAB" . consult-line))
  :init (progn
          (defvar consult-mode-map (make-sparse-keymap))
          (define-minor-mode consult-mode
            "Provide the `consult' commands in a single keymap."
            :global t
            (if consult-mode
                (define-key minibuffer-local-map
                  [remap previous-matching-history-element]
                  #'consult-history)
              (define-key minibuffer-local-map
                [remap previous-matching-history-element]
                nil)))
          (consult-mode 1))
  :config (progn
            (consult-customize
             consult-ripgrep consult-grep
             consult-buffer consult-recent-file
             :preview-key "M-.")

            (defun vifon/orderless-fix-consult-tofu (pattern index total)
              "Ignore the last character which is hidden and used only internally."
              (when (string-suffix-p "$" pattern)
                `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                              "[\x200000-\x300000]*$"))))

            (dolist (command '(consult-buffer consult-line))
              (advice-add command :around
                          (lambda (orig &rest args)
                            (let ((orderless-style-dispatchers (cons #'vifon/orderless-fix-consult-tofu
                                                                     orderless-style-dispatchers)))
                              (apply orig args)))))

            ;; Disable consult-buffer project-related capabilities as
            ;; they are very slow in TRAMP.
            (setq consult-buffer-sources
                  (delq 'consult--source-project-buffer
                        (delq 'consult--source-project-file consult-buffer-sources)))

            (setq consult--source-hidden-buffer
                  (plist-put consult--source-hidden-buffer :narrow ?h))

            (defvar vifon/consult--source-disassociated-file-buffer
              `(:name     "Disassociated File"
                :narrow   ?e
                :category buffer
                :state    ,#'consult--buffer-state
                :items
                ,(lambda ()
                   (consult--buffer-query :sort 'visibility
                                          :as #'buffer-name
                                          :predicate
                                          (lambda (buf)
                                            (let ((file (vifon/buffer-file-or-directory-name buf)))
                                              (and file (not (file-exists-p file)))))))
                "Disassociated buffer candidate source for `consult-buffer'.

Inspired by: `ibuffer-mark-dissociated-buffers'."))
            (defun vifon/consult-disassociated-buffers ()
              "Like `consult-buffer' but only for disassociated buffers."
              (interactive)
              (consult-buffer '(vifon/consult--source-disassociated-file-buffer)))


            (defvar vifon/consult--source-remote-file-buffer
              `(:name     "Remote File"
                :narrow   ?r
                :hidden   t
                :category buffer
                :state    ,#'consult--buffer-state
                :items
                ,(lambda ()
                   (consult--buffer-query :sort 'visibility
                                          :as #'buffer-name
                                          :predicate
                                          (lambda (buf)
                                            (let ((file (vifon/buffer-file-or-directory-name buf)))
                                              (and file (file-remote-p file))))))
                "Remote file buffer candidate source for `consult-buffer'."))
            (add-to-list 'consult-buffer-sources
                         'vifon/consult--source-remote-file-buffer
                         'append)

            ;; Use Consult to select xref locations with preview.
            (setq xref-show-xrefs-function #'consult-xref
                  xref-show-definitions-function #'consult-xref)

            (add-to-list 'consult-bookmark-narrow
                         '(?t "TMSU" tmsu-dired-bookmark-open))))