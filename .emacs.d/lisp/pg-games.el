;;; pg-games.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'sudoku)
(unless (fboundp 'sudoku)
  (autoload #'sudoku "sudoku" nil t))
(with-eval-after-load 'sudoku
  (pg/customize-set-variables
   '((sudoku-style . unicode)
     (sudoku-level . hard))))

(straight-use-package 'sokoban)
(unless (fboundp 'sokoban)
  (autoload #'sokoban "sokoban" nil t))
(unless (fboundp 'sokoban-goto-level)
  (autoload #'sokoban-goto-level "sokoban" nil t))
(with-eval-after-load 'sokoban
  (with-eval-after-load 'evil
    (evil-define-key 'normal sokoban-mode-map
      "h" #'sokoban-move-left
      "l" #'sokoban-move-right
      "j" #'sokoban-move-down
      "k" #'sokoban-move-up)))

(provide 'pg-games)
