;;; pg-programming-python.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'python-mode-hook #'lsp-deferred)
(with-eval-after-load 'python
  (pg/customize-set-variables
   `((python-fill-docstring-style . django)
     (python-shell-virtualenv-path . ,(expand-file-name "~/.conda/envs/ift1016/"))
     (python-indent-offset . 4))))

(defun pg/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(straight-use-package 'conda)
(with-eval-after-load 'conda
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(straight-use-package 'lsp-pyright)
(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'lsp-pyright)))

(with-eval-after-load 'lsp-pyright
  (pg/customize-set-variables
   `((lsp-pyright-venv-path . ,(expand-file-name "~/.conda/envs/ift1016/"))
     (lsp-pyright-venv-directory . "envs"))))

(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'dap-python)))
(with-eval-after-load 'dap-python
  (customize-set-variable 'dap-python-debugger 'debugpy))

(straight-use-package 'jupyter)

(provide 'pg-programming-python)
