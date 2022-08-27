(defun pg/lsp-mode-setup ()
  "Displays structure of cursor position for all buffers."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-lens-mode)
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :init
  (require 'lsp-completion)
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode-hook . pg/lsp-mode-setup)
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-include-signature t))

(use-package lsp-treemacs
  :straight t
  :after lsp)

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Configures company backend with yasnippet for autocomplete candidates."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :straight t
  :hook
  (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-minimum-width 40)
  (company-tooltip-maximum-width 60)
  (company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common)))

(use-package company-box
  :straight t
  :after company
  :hook
  (company-mode . company-box-mode))

(use-package company-prescient
  :straight t
  :after (company prescient)
  :custom
  (company-prescient-mode 1))

(use-package flycheck
  :straight t
  :after lsp-mode
  :hook
  (lsp-mode-hook . flycheck-mode))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :custom
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1))

(use-package plantuml-mode
  :straight t
  :custom
  (plantuml-indent-level 4)
  (plantuml-jar-path "~/bin/plantuml.jar")
  (plantuml-default-exec-mode 'jar))

(use-package comment-dwim-2
  :straight t
  :bind
  ("M-/" . comment-dwim-2)
  (:map org-mode-map
        ("M-/" . org-comment-dwim-2)))

(use-package yasnippet
  :straight t
  :after company
  :diminish yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)
  (yas-minor-mode . (lambda ()
                      (yas-activate-extra-mode 'fundamental-mode)))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

(provide 'pg-programming)
