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
  (ledger-binary-path (concat (getenv "GUIX_EXTRA_PROFILES") "/emacs/emacs/bin/ledger"))
  (ledger-clear-whole-transaction t))

(provide 'pg-finance)
