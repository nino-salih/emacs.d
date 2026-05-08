;;; -*- lexical-binding: t; -*-
;; Corfu is used for autocompletion in the buffer
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*"))
  :init (global-corfu-mode 1)
  :bind
  (:map corfu-map 
     ("C-SPC" . corfu-insert-separator)
     ("C-j"   . corfu-next)
     ("C-k"   . corfu-previous)
     ("M-i"   . corfu-popupinfo-toggle)        ; toggle doc popup
     ("M-o"   . corfu-popupinfo-documentation) ; open doc in own buffer
     ("M-j"   . corfu-popupinfo-scroll-up)     ; scroll doc popup down
     ("M-k"   . corfu-popupinfo-scroll-down)   ; scroll doc popup up
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
  (corfu-popupinfo-delay 0.25)
  :config
  (corfu-popupinfo-mode 1))