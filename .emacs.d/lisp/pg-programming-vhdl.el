(flycheck-define-checker vhdl-tool
  "A VHDL syntax checker, type checker and linter using VHDL-Tool."
  :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source)
  :standard-input t
  :modes (vhdl-mode)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
   (error line-start (file-name) ":" line ":" column ":e:" (message) line-end)))

(straight-use-package 'vhdl-tools)
(require 'vhdl-tools)
(with-eval-after-load 'vhdl-tools
  (add-hook 'vhdl-mode-hook #'lsp-deferred)
  (add-to-list 'flycheck-checkers 'vhdl-tool)
  (customize-set-variable 'lsp-vhdl-server-path "~/.emacs.d/lsp-servers/vhdl-tool"))

(provide 'pg-programming-vhdl)
