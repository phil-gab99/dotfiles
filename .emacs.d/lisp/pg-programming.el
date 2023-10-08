;;; pg-programming.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/lsp-mode-setup ()
  "Displays structure of cursor position for all buffers."
  (lsp-lens-mode)
  (lsp-headerline-breadcrumb-mode))

(straight-use-package 'lsp-mode)
(add-hook 'lsp-mode-hook #'pg/lsp-mode-setup)
(unless (fboundp 'lsp)
  (autoload #'lsp "lsp-mode" nil t))
(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(with-eval-after-load 'lsp
  (require 'lsp-completion)
  (pg/customize-set-variables
   '((lsp-completion-provider . :none)
     (lsp-keymap-prefix . "C-c l")))
  (lsp-enable-which-key-integration t))
(with-eval-after-load 'general
  (pg/leader-keys
    "l" '(:ignore t :which-key "lsp")))

(straight-use-package 'lsp-ui)
(unless (fboundp 'lsp-ui-mode)
  (autoload #'lsp-ui-mode "lsp-ui" nil t))
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
(with-eval-after-load 'lsp-ui
  (pg/customize-set-variables
   '((lsp-ui-doc-position . bottom)
     (lsp-ui-doc-show-with-cursor . t)
     (lsp-ui-doc-include-signature . t))))

(straight-use-package 'lsp-treemacs)
(with-eval-after-load 'lsp-mode
  (require 'lsp-treemacs))
(with-eval-after-load 'lsp-treemacs
  (with-eval-after-load 'general
    (pg/leader-keys
      "lt" '(treemacs :which-key "tree")
      "lo" '(lsp-treemacs-symbols :which-key "outline")
      "le" '(lsp-treemacs-errors-list :which-key "errors"))))

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Configures company backend with yasnippet for autocomplete candidates."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(straight-use-package 'company)
(unless (fboundp 'company-mode)
  (autoload #'company-mode "company" nil t))
(add-hook 'prog-mode-hook #'company-mode)
(unless (fboundp 'company-complete-selection)
  (autoload #'company-complete-selection "company" nil t))
(if (boundp 'company-active-map)
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)))
(with-eval-after-load 'lsp-mode
  (unless (fboundp 'company-indent-or-complete-common)
    (autoload #'company-indent-or-complete-common "company" nil t))
  (define-key lsp-mode-map (kbd "<tab>") #'company-indent-or-complete-common))
(with-eval-after-load 'company
  (pg/customize-set-variables
   '((company-minimum-prefix-length . 1)
     (company-idle-delay . 0.0)
     (company-dabbrev-downcase . nil)
     (company-tooltip-minimum-width . 40)
     (company-tooltip-maximum-width . 60)))
  (with-eval-after-load 'yasnippet
    (customize-set-variable 'company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(straight-use-package 'company-box)
(unless (fboundp 'company-box-mode)
  (autoload #'company-box-mode "company-box" nil t))
(add-hook 'company-mode-hook #'company-box-mode)

(straight-use-package 'company-prescient)
(with-eval-after-load 'company
  (require 'prescient)
  (with-eval-after-load 'prescient
    (require 'company-prescient)))
(with-eval-after-load 'company-prescient
  (customize-set-variable 'company-prescient-mode 1))

(straight-use-package 'flycheck)
(unless (fboundp 'flycheck-mode)
  (autoload #'flycheck-mode "flycheck" nil t))
(add-hook 'lsp-mode-hook #'flycheck-mode)

(straight-use-package 'dap-mode)
(with-eval-after-load 'lsp-mode
  (require 'dap-mode))
(with-eval-after-load 'dap-mode
  (pg/customize-set-variables
   '((dap-mode . 1)
     (dap-ui-mode . 1)
     (dap-ui-controls-mode . 1))))

(straight-use-package 'plantuml-mode)
(with-eval-after-load 'plantuml-mode
  (pg/customize-set-variables
   `((plantuml-indent-level . 4)
     (plantuml-jar-path . ,(expand-file-name "~/.guix-extra-profiles/emacs/emacs/share/java/plantuml.jar"))
     (plantuml-default-exec-mode jar))))

(straight-use-package 'comment-dwim-2)
(unless (fboundp 'comment-dwim-2)
  (autoload #'comment-dwim-2 "comment-dwim-2" nil t))
(global-set-key (kbd "M-/") #'comment-dwim-2)
(unless (fboundp 'org-comment-dwim-2)
  (autoload #'org-comment-dwim-2 "comment-dwim-2" nil t))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-/") #'org-comment-dwim-2))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (gitignore "https://github.com/shunsambongi/tree-sitter-gitignore")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (scheme "https://github.com/6cdh/tree-sitter-scheme")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

(customize-set-variable 'major-mode-remap-alist
                        '((sh-mode . bash-ts-mode)
                          (javascript-mode . tsx-ts-mode)))

(straight-use-package 'yasnippet)
(unless (fboundp 'yas-minor-mode)
  (autoload #'yas-minor-mode "yasnippet" nil t))
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'yas-minor-mode-hook #'(lambda ()
                                   (yas-activate-extra-mode 'fundamental-mode)))
(with-eval-after-load 'yasnippet
  (yas-global-mode)
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'yas-minor-mode)
  (add-to-list 'yas-snippet-dirs "~/Projects/guix/etc/snippets/yas"))

(straight-use-package 'yasnippet-snippets)
(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets))

(provide 'pg-programming)
