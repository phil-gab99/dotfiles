;;; pg-programming-elisp.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'corfu-mode)
  (autoload #'corfu-mode "corfu" nil t))
(add-hook 'ielm-mode-hook #'corfu-mode)
(add-hook 'ielm-mode-hook #'(lambda ()
                              (display-line-numbers-mode 0)))

(provide 'pg-programming-elisp)
