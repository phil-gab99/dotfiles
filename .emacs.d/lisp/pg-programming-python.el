;;; pg-programming-python.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/run-ipython ()
  "Runs an inferior iPython process."
  (interactive)
  (when (executable-find "ipython")
    (ansi-term "ipython")))

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'python-mode-ts-hook #'lsp-deferred)
(add-hook 'inferior-python-mode-hook #'corfu-mode)

(with-eval-after-load 'python
  (pg/customize-set-variables
   `((python-fill-docstring-style . django)
     (python-shell-virtualenv-root . ,(expand-file-name "~/.conda/envs"))
     (define-key python-mode-map (kbd "C-c C-i") #'pg/run-ipython)
     (define-key inferior-python-mode-map (kbd "TAB") #'complete-symbol)
     (python-indent-offset . 4))))

(straight-use-package 'conda)
(when (executable-find "conda")
  (add-hook'conda-postactivate-hook #'(lambda ()
                                        (setenv "OLD_JUPYTER_PATH" (getenv "JUPYTER_PATH"))
                                        (if (null (getenv "JUPYTER_PATH"))
                                            (setenv "JUPYTER_PATH" (concat conda-env-current-path "lib"))
                                          (setenv "JUPYTER_PATH" (concat (getenv "JUPYTER_PATH") ":" conda-env-current-path "lib")))))
  (add-hook 'conda-predeactivate-hook #'(lambda ()
                                          (setenv "JUPYTER_PATH" (getenv "OLD_JUPYTER_PATH"))
                                          (setenv "JUPYTER_PATH")))
  (pg/customize-set-variables
   `((conda-anaconda-home . ,(string-replace "/bin/conda" "" (executable-find "conda")))
     (conda-env-home-directory . ,(expand-file-name "~/.conda/"))
     (conda-env-subdirectory . "envs")))
  (unless (getenv "CONDA_DEFAULT_ENV")
    (conda-env-activate "ml_practice"))
  (with-eval-after-load 'conda
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)))

(straight-use-package 'lsp-pyright)
(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'lsp-pyright)))

(with-eval-after-load 'lsp-pyright
  (pg/customize-set-variables
   `((lsp-pyright-venv-path . ,(expand-file-name "~/.conda/envs/"))
     (lsp-pyright-venv-directory . "envs"))))

(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'dap-python)))
(with-eval-after-load 'dap-python
  (customize-set-variable 'dap-python-debugger 'debugpy))

(defun pg/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(straight-use-package 'jupyter)
(unless pg/is-windows
  (add-hook 'jupyter-repl-mode-hook #'company-mode)
  (add-hook 'jupyter-repl-mode-hook #'(lambda ()
                                        (require 'jupyter))))

(provide 'pg-programming-python)
