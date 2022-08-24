(require 'pg-startup)

(use-package doc-view
  :straight nil
  :mode ("\\.djvu\\'" . doc-view-mode))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-misc-print-programm "/usr/bin/lpr")
  (pdf-misc-print-programm-args '("-o sides=two-sided-long-edge")))

(use-package djvu
  :straight t)

(use-package ps-print
  :straight nil
  :bind
  (:map pdf-view-mode-map
        ("C-c C-p" . pdf-misc-print-document))
  :config
  (require 'pdf-misc)
  :custom ;; Printing double-sided
  (ps-lpr-switches '("-o sides=two-sided-long-edge"))
  (ps-spool-duplex t))

(provide 'pg-viewers)
