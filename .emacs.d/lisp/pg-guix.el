(require 'pg-startup)

(use-package guix
  :straight nil)

(use-package geiser
  :straight nil
  :bind (:map geiser-repl-mode-map
              ("C-l" . geiser-repl-clear-buffer)))

(provide 'pg-guix)
