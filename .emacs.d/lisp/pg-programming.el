(defun pg/lsp-mode-setup ()
  "Displays structure of cursor position for all buffers."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-lens-mode)
  (lsp-headerline-breadcrumb-mode))

(straight-use-package 'lsp-mode)
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

(straight-use-package 'lsp-ui)
(with-eval-after-load 'lsp-mode
  (require 'lsp-ui)
  (with-eval-after-load 'lsp-ui
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    (customize-set-variable 'lsp-ui-doc-position 'bottom)
    (customize-set-variable 'lsp-ui-doc-show-with-cursor t)
    (customize-set-variable 'lsp-ui-doc-include-signature t)))

(straight-use-package 'lsp-treemacs)
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

(straight-use-package 'company)
(with-eval-after-load 'lsp-mode
  (require 'company (expand-file-name "straight/repos/company-mode/company.el" user-emacs-directory))
  (with-eval-after-load 'company
    (add-hook 'prog-mode-hook #'company-mode)
    (define-key company-active-map "<tab>" #'company-complete-selection)
    (define-key lsp-mode-map "<tab>" #'company-indent-or-complete-common)
    (customize-set-variable 'company-minimum-prefix-length 1)
    (customize-set-variable 'company-idle-delay 0.0)
    (customize-set-variable 'company-tooltip-minimum-width 40)
    (customize-set-variable 'company-tooltip-maximum-width 60)
    (customize-set-variable 'company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(straight-use-package 'company-box)
(with-eval-after-load 'company
  (require 'company-box (expand-file-name "straight/repos/company-box/company-box.el" user-emacs-directory))
  (with-eval-after-load 'company-box
    (add-hook 'company-mode-hook #'company-box-mode)))

(straight-use-package 'company-prescient)
(with-eval-after-load 'company
  (with-eval-after-load 'prescient
    (require 'company-prescient)
    (with-eval-after-load 'company-prescient
      (company-prescient-mode 1))))

(straight-use-package 'flycheck)
(with-eval-after-load 'lsp-mode
  (require 'flycheck)
  (with-eval-after-load 'flycheck
    (add-hook 'lsp-mode-hook #'flycheck-mode)))

(straight-use-package 'dap-mode)
(with-eval-after-load 'lsp-mode
  (require 'dap-mode)
  (with-eval-after-load 'dap-mode
    (customize-set-variable 'dap-mode 1)
    (customize-set-variable 'dap-ui-mode 1)
    (customize-set-variable 'dap-ui-controls-mode 1)))

(straight-use-package 'plantuml-mode)
(require 'plantuml-mode)
(with-eval-after-load 'plantuml-mode
  (customize-set-variable 'plantuml-indent-level 4)
  (customize-set-variable 'plantuml-jar-path "~/bin/plantuml.jar")
  (customize-set-variable 'plantuml-default-exec-mode 'jar))

(straight-use-package 'comment-dwim-2)
(unless (fboundp 'comment-dwim-2)
  (autoload #'comment-dwim-2 "comment-dwim-2" nil t))
(unless (fboundp 'org-comment-dwim-2)
  (autoload #'org-comment-dwim-2 "comment-dwim-2" nil t))
(require 'comment-dwim-2)
(with-eval-after-load 'comment-dwim-2
  (global-set-key (kbd "M-/") #'comment-dwim-2)
  (define-key org-mode-map (kbd "M-/") #'org-comment-dwim-2))

(straight-use-package 'yasnippet)
(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'yas-minor-mode-hook #'(lambda () (yas-activate-extra-mode 'fundamental-mode)))
  (yas-global-mode 1))

(straight-use-package 'yasnippet-snippets)
(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets))

(provide 'pg-programming)
