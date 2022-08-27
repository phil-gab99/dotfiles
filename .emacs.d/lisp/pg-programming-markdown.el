(use-package markdown-mode
  :straight t
  :hook
  (gfm-view-mode . (lambda ()
                     (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                                        (markdown-code-face (:height 1.5) fixed-pitch))))))

(provide 'pg-programming-markdown)
