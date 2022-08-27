;;; pg-finance.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package ledger-mode
  :straight t
  :init
  (require 'ledger-mode)
  :mode "\\.dat$"
  :hook
  (ledger-mode . company-mode)
  :custom
  (ledger-reconcile-default-commodity "CAD")
  (ledger-binary-path (expand-file-name "~/.guix-extra-profiles/emacs/emacs/bin/ledger"))
  (ledger-clear-whole-transaction t))

(provide 'pg-finance)
