(defun pg/lsp-mode-setup () ; Displays structure of cursor position for all buffers
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-lens-mode)
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . pg/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (require 'lsp-completion)
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-completion-provider :none))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
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
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :straight t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-minimum-width 40)
  (company-tooltip-maximum-width 60)
  :config
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-box
  :straight t
  :after company
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :straight t
  :after company
  :config
  (company-prescient-mode 1))

(use-package flycheck
  :straight t
  :hook (lsp-mode . flycheck-mode))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1))

;;(general-define-key
;;  :keymaps 'lsp-mode-map
;;  :prefix lsp-keymap-prefix
;;  "d" '(dap-hydra t :wk "debugger")))

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
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1)
  (add-hook 'yas-minor-mode-hook (lambda ()
                                   (yas-activate-extra-mode 'fundamental-mode))))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(provide 'pg-programming)
