;;; pg-programming-gherkin.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/disable-lsp-completion ()
  (setq-local lsp-completion-enable nil))

(straight-use-package 'feature-mode)
(dolist (fn #'(lsp-deferred
               company-mode
               pg/disable-lsp-completion))
  (add-hook 'feature-mode-hook fn))
(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))

(provide 'pg-programming-gherkin)
