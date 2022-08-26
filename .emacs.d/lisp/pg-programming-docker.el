(straight-use-package 'docker)
(require 'docker)

(straight-use-package 'dockerfile-mode)
(with-eval-after-load 'docker
  (require 'dockerfile-mode))

(provide 'pg-programming-docker)
