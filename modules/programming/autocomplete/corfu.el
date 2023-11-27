;;; -*- lexical-binding: t; -*-
;; Corfu is used for autocompletion in the buffer
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*"))
  :init (global-corfu-mode 1)
  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map 
     ("M-SPC" . corfu-insert-separator)
     ("C-j" . corfu-next)
     ("C-k" . corfu-previous)
  )
  :custom 
  (completion-cycle-threshold nil)
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  ;;(corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-min-width 50)
  (corfu-max-width corfu-min-width) ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-popupinfo-mode 1)
  (corfu-popupinfo-delay 0.25))