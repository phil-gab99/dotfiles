(unless (fboundp 'guix)
  (autoload #'guix "guix" nil t))

(unless (fboundp 'run-geiser)
  (autoload #'run-geiser "geiser-repl" nil t))
(with-eval-after-load 'geiser-repl
  (define-key geiser-repl-mode-map "C-l" #'geiser-repl-clear-buffer))

(provide 'pg-guix)
