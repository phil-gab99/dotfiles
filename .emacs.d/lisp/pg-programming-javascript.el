;;; pg-programming-javascript.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'javascript-mode)
  (autoload #'javascript-mode "js" nil t))
(with-eval-after-load 'js
  (customize-set-variable 'js-indent-level 2))

(provide 'pg-programming-javascript)
