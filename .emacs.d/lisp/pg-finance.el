(use-package ledger-mode
  :straight t
  :mode "\\.dat\\'"
  :hook (ledger-mode . company-mode)
  :custom
  (ledger-reconcile-default-commodity "CAD")
  (ledger-binary-path "/home/phil-gab99/.guix-extra-profiles/emacs/emacs/bin/ledger")
  (ledger-clear-whole-transaction t))

(provide 'pg-finance)
