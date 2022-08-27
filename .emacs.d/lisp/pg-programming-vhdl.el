;;; pg-programming-vhdl.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(flycheck-define-checker vhdl-tool
  "A VHDL syntax checker, type checker and linter using VHDL-Tool."
  :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source)
  :standard-input t
  :modes (vhdl-mode)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
   (error line-start (file-name) ":" line ":" column ":e:" (message) line-end)))

(use-package vhdl-tools
  :disabled
  :straight t
  :init
  (require 'vhdl-tools)
  :after flycheck
  :hook
  (vhdl-mode . lsp-deferred)
  :config
  (add-to-list 'flycheck-checkers 'vhdl-tool)
  :custom
  (lsp-vhdl-server-path "~/.emacs.d/lsp-servers/vhdl-tool"))

(provide 'pg-programming-vhdl)
