(require 'pg-startup)

(use-package alloy-mode
  :straight nil
  :hook (alloy-mode . (lambda () (setq indent-tabs-mode nil)))
  :load-path "~/.emacs.d/extrapkgs/alloy-mode"
  :custom
  (alloy-basic-offset 4))

(provide 'pg-programming-alloy)
