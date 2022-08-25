(defun pg/lsp-mode-setup ()
  "Displays structure of cursor position for all buffers."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-lens-mode)
  (lsp-headerline-breadcrumb-mode))

(unless (fboundp 'lsp)
  (autoload #'lsp "lsp-mode" nil t))
(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'pg/lsp-mode-setup)
  (require 'lsp-completion)
  (lsp-enable-which-key-integration t)
  (customize-set-variable 'lsp-completion-provider :none)
  (customize-set-variable 'lsp-keymap-prefix "C-c l"))

(with-eval-after-load 'lsp-mode
  (require 'lsp-ui)
  (with-eval-after-load 'lsp-ui
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    (customize-set-variable 'lsp-ui-doc-position 'bottom)
    (customize-set-variable 'lsp-ui-doc-show-with-cursor t)
    (customize-set-variable 'lsp-ui-doc-include-signature t)))

(with-eval-after-load 'lsp-mode
  (require 'lsp-treemacs))

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Configures company backend with yasnippet for autocomplete candidates."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(with-eval-after-load 'lsp-mode
  (require 'company)
  (with-eval-after-load 'company
    (add-hook 'prog-mode-hook #'company-mode)
    (bind-keys :package company :map company-active-map
               ("<tab>" . company-complete-selection)
               :map lsp-mode-map
               ("<tab>" . company-indent-or-complete-common))
    (customize-set-variable 'company-minimum-prefix-length 1)
    (customize-set-variable 'company-idle-delay 0.0)
    (customize-set-variable 'company-tooltip-minimum-width 40)
    (customize-set-variable 'company-tooltip-maximum-width 60)
    (customize-set-variable 'company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(with-eval-after-load 'company
  (require 'company-box)
  (with-eval-after-load 'company-box
    (add-hook 'company-mode-hook #'company-box-mode)))

(with-eval-after-load 'company
  (straight-use-package 'company-prescient
  (require 'company-prescient)
  (with-eval-after-load 'company-prescient
    (company-prescient-mode 1))))

(with-eval-after-load 'lsp-mode
  (require 'flycheck)
  (with-eval-after-load 'flycheck
    (add-hook 'lsp-mode-hook #'flycheck-mode)))

(with-eval-after-load 'lsp-mode
  (require 'dap-mode)
  (with-eval-after-load 'dap-mode
    (customize-set-variable 'dap-mode 1)
    (customize-set-variable 'dap-ui-mode 1)
    (customize-set-variable 'dap-ui-controls-mode 1)))

(require 'plantuml-mode)
(with-eval-after-load 'plantuml-mode
  (customize-set-varaible 'plantuml-indent-level 4)
  (customize-set-varaible 'plantuml-jar-path "~/bin/plantuml.jar")
  (customize-set-varaible 'plantuml-default-exec-mode 'jar))

(straight-use-package 'comment-dwim-2)
(unless (fboundp 'comment-dwim-2)
  (autoload #'comment-dwim-2 "comment-dwim-2" nil t))
(unless (fboundp 'org-comment-dwim-2)
  (autoload #'org-comment-dwim-2 "comment-dwim-2" nil t))
(require 'comment-dwim-2)
(with-eval-after-load 'comment-dwim-2
  (bind-keys :package comment-dwim-2
                   ("M-/" . comment-dwim-2)
                   :map org-mode-map
                   ("M-/" . org-comment-dwim-2)))

(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'yas-minor-mode-hook #'(lambda () (yas-activate-extra-mode 'fundamental-mode)))
  (yas-global-mode 1))

(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets))

(provide 'pg-programming)
