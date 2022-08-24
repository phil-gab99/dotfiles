(require 'pg-startup)

(flycheck-define-checker vhdl-tool
  "A VHDL syntax checker, type checker and linter using VHDL-Tool."
  :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source)
  :standard-input t
  :modes (vhdl-mode)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
   (error line-start (file-name) ":" line ":" column ":e:" (message) line-end)))

(use-package vhdl-tools
  :straight t
  :disabled ;; Settings and binaries not configured
  :hook (vhdl-mode . lsp-deferred)
  :custom
  (lsp-vhdl-server-path "~/.emacs.d/lsp-servers/vhdl-tool")
  :config
  (add-to-list 'flycheck-checkers 'vhdl-tool))

(provide 'pg-programming-vhdl)
