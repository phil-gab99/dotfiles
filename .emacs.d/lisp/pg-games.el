(straight-use-package 'sudoku)
(unless (fboundp 'sudoku)
  (autoload #'sudoku "sudoku" nil t))
(with-eval-after-load 'sudoku
  (customize-set-variable 'sudoku-style 'unicode)
  (customize-set-variable 'sudoku-level 'hard))

(straight-use-package 'sokoban)
(unless (fboundp 'sokoban)
  (autoload #'sokoban "sokoban" nil t))
(unless (fboundp 'sokoban-goto-level)
  (autoload #'sokoban-goto-level "sokoban" nil t))
(with-eval-after-load 'sokoban
  (define-key sokoban-mode-map "<normal-state> h" #'sokoban-move-left)
  (define-key sokoban-mode-map "<normal-state> l" #'sokoban-move-right)
  (define-key sokoban-mode-map "<normal-state> k" #'sokoban-move-up)
  (define-key sokoban-mode-map "<normal-state> j" #'sokoban-move-down))

(provide 'pg-games)
