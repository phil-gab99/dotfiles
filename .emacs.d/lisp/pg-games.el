;;; pg-games.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package sudoku
  :straight t
  :init
  (require 'sudoku)
  :commands sudoku
  :custom
  (sudoku-style 'unicode)
  (sudoku-level 'hard))

(use-package sokoban
  :straight t
  :init
  (require 'sokoban)
  :after evil
  :commands (sokoban sokoban-goto-level)
  :config
  (evil-define-key 'normal sokoban-mode-map
    "h" 'sokoban-move-left
    "l" 'sokoban-move-right
    "j" 'sokoban-move-down
    "k" 'sokoban-move-up))

(provide 'pg-games)
