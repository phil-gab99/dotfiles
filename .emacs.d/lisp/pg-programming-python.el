(require 'pg-startup)

(use-package python-mode
  :straight nil
  :hook (python-mode . lsp-deferred)
  :custom
  ;;(python-shell-interpreter "python3")
  ;;(dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-python-ms
  :straight t
  :init (setq lsp-python-ms-auto-install-server t)
  :custom
  (lsp-python-ms-executable
   "~/.emacs.d/lsp-servers/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp-deferred))))

(use-package jupyter
  :disabled) ;; Figure it out

(provide 'pg-programming-python)
