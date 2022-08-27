;;; pg-viewers.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package doc-view
  :straight nil
  :init
  (require 'doc-view)
  :mode "\\.djvu$")

(use-package pdf-tools
  :straight t
  :init
  (require 'pdf-tools)
  :mode "\\.pdf$")

(use-package djvu
  :straight t
  :init
  (require 'djvu))

(provide 'pg-viewers)
