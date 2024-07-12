;;; pg-programming-haskell.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'haskell-mode)
(with-eval-after-load 'haskell-mode
  (setopt haskell-process-type 'ghci))

(provide 'pg-programming-haskell)
