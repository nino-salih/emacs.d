;;; -*- lexical-binding: t; -*-

;; use-package with package.el:
(use-package dashboard
  :straight t
  :after (nerd-icons)
  :config
  (dashboard-setup-startup-hook)
  :custom
  ;; Set the title
  (dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (dashboard-startup-banner (concat user-emacs-directory "banner.txt"))
  ;; Value can be
  ;; - nil to display no banner
  ;; - 'official which displays the official emacs logo
  ;; - 'logo which displays an alternative emacs logo
  ;; - 1, 2 or 3 which displays one of the text banners
  ;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
  ;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

  ;; Content is not centered by default. To center, set
  (dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (dashboard-show-shortcuts t)
  

  ;; Set the Icons
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)

  ;; Navigator disabled — add real URLs here if needed
  (dashboard-set-navigator nil)

  (dashboard-projects-backend 'project-el)
  ;; Customize which sections appear in the startup screen
  (dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  )


(eval-after-load 'dashboard
  (custom-set-faces
  '(dashboard-text-banner ((t (:weight thin :slant italic :foreground "#FF5D5D" :line-spacing nil))))))