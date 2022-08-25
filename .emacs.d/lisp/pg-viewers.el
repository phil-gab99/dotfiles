(require 'doc-view)
(with-eval-after-load 'doc-view
  (add-to-list 'auto-mode-alist '("\\.djvu\\'" . doc-view-mode)))

(require 'pdf-tools)
(with-eval-after-load 'pdf-tools
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(require 'djvu)

(provide 'pg-viewers)
