(straight-use-package 'mips-mode)
(require 'mips-mode)
(with-eval-after-load 'mips-mode
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . mips-mode))
  (customize-set-variable 'mips-tab-width 4))

(provide 'pg-programming-mips)
