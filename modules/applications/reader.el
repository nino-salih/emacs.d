;; Will be a drop-in replacement for doc-view, nov.el, and pdf-tools
;; Well-supported Formats
;;
;;  PDF
;;  EPUB
;;  MOBI
;;  FB2
;;  XPS/OpenXPS
;;  CBZ

;; Only for Reading
;; ODT
;; ODS
;; ODP
;; ODG
;;
;; TODO: If release is stable remove codberg and use MELPA instead

(use-package reader
    :straight '(reader :type git :host codeberg :repo "MonadicSheep/emacs-reader"
            :files (:defaults "render-core.so")
            :pre-build ("make" "all"))
    :ensure-system-package mupdf)