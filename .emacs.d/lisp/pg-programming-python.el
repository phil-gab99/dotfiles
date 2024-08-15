;;; pg-programming-python.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/run-ipython ()
  "Runs an inferior iPython process."
  (interactive)
  (when (executable-find "ipython")
    (ansi-term "ipython")))

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'python-ts-mode-hook #'lsp-deferred)
(add-hook 'inferior-python-mode-hook #'corfu-mode)
(add-hook 'inferior-python-mode-hook #'(lambda ()
                                         (display-line-numbers-mode 0)))

(with-eval-after-load 'python
  (setopt python-fill-docstring-style 'django
          python-shell-virtualenv-root (concat (plist-get pg/user :home) "/.conda/envs")
          python-indent-offset 4)
  (define-key python-mode-map (kbd "C-c C-i") #'pg/run-ipython)
  (define-key inferior-python-mode-map (kbd "TAB") #'complete-symbol))

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
  (setopt conda-anaconda-home (concat (plist-get pg/user :guix-home-profile))
          conda-env-home-directory (concat (plist-get pg/user :home) "/.conda")
          conda-env-subdirectory "envs")
  (conda-env-activate "ml_practice")
  (with-eval-after-load 'conda
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)))

(straight-use-package 'lsp-pyright)
(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'lsp-pyright)))

(with-eval-after-load 'lsp-pyright
  (setopt lsp-pyright-venv-path (concat (plist-get pg/user :home) "/.conda/envs")
          lsp-pyright-venv-directory "envs"))

(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'dap-python)))
(with-eval-after-load 'dap-python
  (setopt dap-python-debugger 'debugpy))

(defun pg/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(unless pg/is-windows
  (straight-use-package 'jupyter)
  (add-hook 'jupyter-repl-mode-hook #'company-mode)
  (add-hook 'jupyter-repl-mode-hook #'(lambda ()
                                        (display-line-numbers-mode 0)
                                        (require 'jupyter))))

(provide 'pg-programming-python)
