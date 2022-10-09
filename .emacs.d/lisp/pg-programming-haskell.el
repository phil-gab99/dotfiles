;;; pg-programming-haskell.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'haskell-mode)
(with-eval-after-load 'haskell-mode
  (customize-set-variable 'haskell-process-type 'ghci))

;; (straight-use-package 'lsp-haskell)
;; (with-eval-after-load 'haskell-mode
;;   (require 'lsp-haskell))
;; (with-eval-after-load 'lsp-haskell
;;   (dolist (mode '(haskell-mode-hook
;;                   haskell-literate-mode-hook))
;;     (add-hook mode #'lsp-deferred)))

(provide 'pg-programming-haskell)
