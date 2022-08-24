(require 'pg-startup)

(use-package sudoku
  :straight t
  :custom
  (sudoku-style 'unicode)
  (sudoku-level 'hard))

(use-package sokoban
  :straight t
  :bind
  (:map sokoban-mode-map
        ("<normal-state> h" . sokoban-move-left)
        ("<normal-state> l" . sokoban-move-right)
        ("<normal-state> k" . sokoban-move-up)
        ("<normal-state> j" . sokoban-move-down)))

(provide 'pg-games)
