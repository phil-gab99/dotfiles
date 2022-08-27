(use-package ledger-mode
  :straight t
  :mode "\\.dat$"
  :hook
  (ledger-mode . company-mode)
  :custom
  (customize-set-variable 'ledger-reconcile-default-commodity "CAD")
  (customize-set-variable 'ledger-binary-path (expand-file-name "~/.guix-extra-profiles/emacs/emacs/bin/ledger"))
  (customize-set-variable 'ledger-clear-whole-transaction t))

(provide 'pg-finance)
