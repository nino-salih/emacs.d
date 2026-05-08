;;; -*- lexical-binding: t; -*-

(set-frame-font "MonoLisa Nerd Font Mono 12" nil t)


;; disable cursor on none active windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; C-x 1 (delete-other-windows) is the nuclear option – it nukes your entire window layout to focus on one buffer. Then you spend the next minute recreating the layout you just destroyed.
;;
;; With winner-mode and a small wrapper, you can make C-x 1 toggle: press it once to go single-window, press it again to restore the previous layout:
(winner-mode +1)

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)