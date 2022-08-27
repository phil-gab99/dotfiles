(use-package guix
  :straight nil
  :commands guix)

(use-package geiser
  :straight nil
  :commands run-geiser
  :bind
  (:map geiser-repl-mode-map
        ("C-l" . geiser-repl-clear-buffer)))

(provide 'pg-guix)
