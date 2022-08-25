(require 'cc-mode)
(with-eval-after-load 'cc-mode
  (dolist (mode '(c-mode
                  c++-mode
                  objc-mode))
   (add-hook mode #'lsp-deferred))
  (customize-set-variable 'company-clang-executable (expand-file-name "~/.guix-extra-profiles/cc/cc/bin/clang"))
  (require 'cc-vers)
  (with-eval-after-load 'cc-vars
    (customize-set-variable 'c-basic-offset 4)))

(straight-use-package 'company-c-headers)
(with-eval-after-load 'cc-mode
  (with-eval-after-load 'company
    (require 'company-c-headers)
    (with-eval-after-load 'company-c-headers
      (add-to-list 'company-backends '(company-c-headers :with company-yasnippet)))))

(with-eval-after-load 'cc-mode
  (with-eval-after-load 'lsp-mode
    (require 'ccls)))

(provide 'pg-programming-cc)
