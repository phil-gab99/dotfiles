(require 'pg-startup)

(use-package savehist
  :straight nil
  :custom
  (savehist-mode))

(use-package marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

(use-package consult
  :straight t
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  (:map minibuffer-local-map
        ("C-r" . consult-history)))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (orderless-skip-highlighting nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :straight t
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :custom
  (corfu-cycle t))

(use-package vertico
  :straight t
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package embark
  :straight t
  :bind
  ("C-S-a" . embark-act)
  (:map minibuffer-local-map
        ("C-d" . embark-act))
  :custom
  (embark-confirm-act-all nil)
  :config
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :straight '(embark-consult :host github
                             :repo "oantolin/embark"
                             :files ("embark-consult.el"))
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package prescient
  :straight t)

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1)) ; Delay before popup in seconds

(use-package helm
  :straight t
  :after lsp-java
  :bind
  (:map helm-map
        ("C-k" . helm-previous-line)
        ("C-j" . helm-next-line))
  :config
  (helm-mode 1))

(provide 'pg-completion)
