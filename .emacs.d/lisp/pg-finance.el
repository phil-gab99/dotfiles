(straight-use-package 'ledger-mode)
(require 'ledger-mode)
(with-eval-after-load 'ledger-mode
  (add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))
  (add-hook 'ledger-mode-hook #'company-mode)
  (customize-set-variable 'ledger-reconcile-default-commodity "CAD")
  (customize-set-variable 'ledger-binary-path "/home/phil-gab99/.guix-extra-profiles/emacs/emacs/bin/ledger")
  (customize-set-variable 'ledger-clear-whole-transaction t))

(provide 'pg-finance)
