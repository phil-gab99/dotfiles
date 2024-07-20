;;; pg-programming-prolog.el -*- lexical-binding: t; -*-

(add-hook 'prolog-inferior-mode-hook #'(lambda ()
                                         (display-line-numbers-mode 0)))
(unless (fboundp 'prolog-mode)
  (autoload #'prolog-mode "prolog" nil t))

(provide 'pg-programming-prolog)
