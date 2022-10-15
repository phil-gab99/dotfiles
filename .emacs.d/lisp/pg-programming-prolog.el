;;; pg-programming-prolog.el -*- lexical-binding: t; -*-

(unless (fboundp 'prolog-mode)
  (autoload #'prolog-mode "prolog" nil t))

(provide 'pg-programming-prolog)
