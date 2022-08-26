(require 'doc-view)
(with-eval-after-load 'doc-view
  (add-to-list 'auto-mode-alist '("\\.djvu\\'" . doc-view-mode)))

(straight-use-package 'pdf-tools)
(require 'pdf-tools)
(with-eval-after-load 'pdf-tools
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(straight-use-package 'djvu)
(require 'djvu)

(provide 'pg-viewers)
