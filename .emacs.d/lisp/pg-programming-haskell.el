;;; pg-programming-haskell.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(add-hook 'inferior-haskell-mode-hook #'(lambda ()
                                          (display-line-numbers-mode 0)))
(straight-use-package 'haskell-mode)
(with-eval-after-load 'haskell-mode
  (setopt haskell-process-type 'ghci))

(provide 'pg-programming-haskell)
