;;; pg-completion.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package savehist
  :straight nil
  :init
  (require 'savehist)
  :config
  (savehist-mode))

(use-package marginalia
  :straight t
  :init
  (require 'marginalia)
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :config
  (marginalia-mode))

(use-package consult
  :straight t
  :init
  (require 'consult)
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  (:map minibuffer-local-map
        ("C-r" . consult-history)))

(use-package orderless
  :straight t
  :init
  (require 'orderless)
  :after vertico
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (orderless-skip-highlighting nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :straight t
  :init
  (require 'corfu)
  :custom
  (corfu-cycle t)
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous)))

(use-package vertico
  :straight t
  :init
  (require 'vertico)
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous))
  :config
  (vertico-mode))

(use-package embark
  :straight t
  :init
  (require 'embark)
  :custom
  (embark-confirm-act-all nil)
  :bind
  ("C-S-a" . embark-act)
  (:map minibuffer-local-map
        ("C-d" . embark-act))
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
  :init
  (require 'embark-consult)
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package prescient
  :straight t
  :init
  (require 'prescient))

(use-package which-key
  :straight t
  :init
  (require 'which-key)
  :after diminish
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

(defun pg/helm-lsp-java ()
  "Enables `helm' when `lsp-java' is running."
  (if (and (eq #'java-mode major-mode)
           (memq #'lsp-mode local-minor-modes))
      (helm-mode 1)
    (helm-mode 0)))

(use-package helm
  :straight t
  :init
  (require 'helm)
  :after lsp-java
  :hook
  (java-mode . pg/helm-lsp-java)
  :bind
  (:map helm-map
        ("C-j" . helm-next-line)
        ("C-k" . helm-previous-line)))

(provide 'pg-completion)
