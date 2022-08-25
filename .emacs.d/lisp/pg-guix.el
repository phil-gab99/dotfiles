(unless (fboundp 'guix)
  (autoload #'guix "guix" nil t))

(unless (fboundp 'run-geiser)
  (autoload #'run-geiser "geiser-repl" nil t))
(with-eval-after-load 'geiser-repl
  (bind-key "C-l" #'geiser-repl-clear-buffer geiser-repl-mode-map))

(provide 'pg-guix)
