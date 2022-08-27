(use-package alloy-mode
  :disabled
  :straight t
  :hook
  (alloy-mode . (lambda ()
                  (setq indent-tabs-mode nil)))
  :custom
  (alloy-basic-offset 4))

(provide 'pg-programming-alloy)
