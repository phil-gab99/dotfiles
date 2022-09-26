;;; pg-finance.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'ledger-mode)
(unless (fboundp 'ledger-mode)
  (autoload #'ledger-mode "ledger-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))
(with-eval-after-load 'ledger-mode
  (pg/customize-set-variables
   `((ledger-reconcile-default-commodity . "CAD")
     (ledger-binary-path . ,(concat (getenv "GUIX_EXTRA_PROFILES") "/emacs/emacs/bin/ledger"))
     (ledger-clear-whole-transaction . t)))
  (unless (fboundp 'company-mode)
    (autoload #'company-mode "company" nil t))
  (add-hook 'ledger-mode-hook #'company-mode))

(provide 'pg-finance)
