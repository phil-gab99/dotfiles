;;; pg-viewers.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'doc-view-mode)
  (autoload #'doc-view-mode "doc-view" nil t))
(add-to-list 'auto-mode-alist '("\\.djvu$" . doc-view-mode))

(straight-use-package 'pdf-tools)
(unless (fboundp 'pdf-view-mode)
  (autoload #'pdf-view-mode "pdf-tools" nil t))
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-view-mode))

(straight-use-package 'djvu)
(require 'djvu)

(provide 'pg-viewers)
