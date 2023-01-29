;;; pg-programming-cc.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(with-eval-after-load 'cc-mode
  (customize-set-variable 'company-clang-executable (concat (getenv "GUIX_EXTRA_PROFILES") "/cc/cc/bin/clang")))

(with-eval-after-load 'cc-mode
  (require 'cc-vars))
(with-eval-after-load 'cc-vars
  (customize-set-variable 'c-basic-offset 4))

(defun pg/company-c-headers-get-system-path ()
  "Return the system include path for the current buffer."
  (let ((default `(,(expand-file-name "~/.guix-extra-profiles/cc/cc/include/"))))
    (company-arduino-append-include-dirs default t)))

(straight-use-package 'company-c-headers)
(with-eval-after-load 'company
  (with-eval-after-load 'cc-mode
    (require 'company-c-headers)))
(with-eval-after-load 'company-c-headers
  (customize-set-variable 'company-c-headers-path-system 'pg/company-c-headers-get-system-path)
  (add-to-list 'company-backends '(company-c-headers :with company-yasnippet)))

(straight-use-package 'ccls)
(with-eval-after-load 'cc-mode
  (with-eval-after-load 'lsp-mode
    (require 'ccls)))
(dolist (mode '(c-mode-hook
                c++-mode-hook
                objc-mode-hook))
  (add-hook mode #'lsp-deferred))

(straight-use-package 'irony)
(dolist (mode '(c-mode-hook
                c++-mode-hook
                objc-mode-hook))
  (add-hook mode #'irony-mode))
(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

(straight-use-package 'company-irony)

(provide 'pg-programming-cc)
