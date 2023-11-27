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

  ;; Show the navigation buttons
  (dashboard-set-navigator t)


  ;; Format: "(icon title help action face prefix suffix)"
  (dashboard-navigator-buttons
        `(;; line1
          ((,(nerd-icons-faicon "nf-fa-github" :height 1.1 :v-adjust 0.0)
          "Homepage"
          "Browse homepage"
          (lambda (&rest _) (browse-url "homepage")))
          ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
          ("?" "" "?/h" #'show-help nil "<" ">"))
          ;; line 2
          ((,(nerd-icons-faicon "nf-fa-linkedin_square" :height 1.1 :v-adjust 0.0)
            "Linkedin"
            ""
            (lambda (&rest _) (browse-url "homepage")))
          ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))

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