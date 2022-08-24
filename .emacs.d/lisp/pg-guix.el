(require 'guix)

(require 'geiser)
(with-eval-after-load 'geiser-repl
  (bind-key "C-l" #'geiser-repl-clear-buffer geiser-repl-mode-map))

(provide 'pg-guix)
