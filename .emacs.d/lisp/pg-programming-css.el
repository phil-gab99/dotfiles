(with-eval-after-load 'lsp-mode
  (require 'lsp-css)
  (with-eval-after-load 'lps-css
    (dolist (mode '(css-mode
                    less-css-mode
                    scss-mode))
      (add-hook mode #'lsp-deferred))))

(provide 'pg-programming-css)
