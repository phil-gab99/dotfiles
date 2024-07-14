;;; pg-finance.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'ledger-mode)
  (autoload #'ledger-mode "ledger-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))
(unless (fboundp 'company-mode)
  (autoload #'company-mode "company" nil t))
(add-hook 'ledger-mode-hook #'company-mode)
(with-eval-after-load 'ledger-mode
  (setopt ledger-reconcile-default-commodity "CAD"
          ledger-binary-path (concat (plist-get pg/user :guix-home-profile) "/bin/ledger")
          ledger-clear-whole-transaction t))

(provide 'pg-finance)
