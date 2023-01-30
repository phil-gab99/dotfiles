;;; pg-programming-elisp.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'corfu-mode)
  (autoload #'corfu-mode "corfu" nil t))
(add-hook 'ielm-mode-hook #'corfu-mode)

(provide 'pg-programming-elisp)
