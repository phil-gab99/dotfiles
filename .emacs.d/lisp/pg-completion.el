(require 'savehist)
(with-eval-after-load 'savehist
  (savehist-mode))

(require 'vertico)
(require 'marginalia)

(with-eval-after-load 'marginalia
  (customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode))

(require 'consult)
(with-eval-after-load 'consult
  (bind-key "C-s" #'consult-line)
  (bind-key "C-x b" #'consult-buffer)
  (bind-key "C-r" #'consult-history minibuffer-local-map))

(require 'orderless)
(with-eval-after-load 'orderless
  (customize-set-variable 'completion-styles '(orderless))
  (customize-set-variable 'completion-category-defaults nil)
  (customize-set-variable 'orderless-skip-highlighting nil)
  (customize-set-variable 'completion-category-overrides '((file (styles basic partial-completion)))))

(require 'corfu)
(with-eval-after-load 'corfu
  (bind-key "C-j" #'corfu-next corfu-map)
  (bind-key "C-k" #'corfu-previous corfu-map)
  (customize-set-variable 'corfu-cycle t))

(require 'vertico)
(with-eval-after-load 'corfu
  (bind-key "C-j" #'vertico-next vertico-map)
  (bind-key "C-k" #'vertico-previous vertico-map)
  (customize-set-variable 'vertico-cycle t)
  (vertico-mode))

(require 'embark)
(with-eval-after-load 'embark
  (bind-key "C-S-a" #'embark-act)
  (bind-key "C-d" #'embark-act minibuffer-local-map)
  (customize-set-variable 'embark-confirm-act-all nil)
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(require 'embark)
(require 'consult)
(straight-use-package '(embark-consult :host github
                                       :repo "oantolin/embark"
                                       :files ("embark-consult.el")))
(require 'embark-consult)
(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook consult-preview-at-point-mode))

(require 'prescient)

(require 'which-key)
(with-eval-after-load 'which-key
  (which-key-mode)
  (customize-set-variable 'which-key-idle-delay 1)
  (diminish 'which-key-mode))

(require 'helm)
(with-eval-after-load 'helm
  (bind-key "C-j" #'helm-next-line helm-map)
  (bind-key "C-k" #'helm-previous-line helm-map)
  (if (and (eq #'java-mode major-mode) (memq #'lsp-mode local-minor-modes))
      (helm-mode 1)
    (helm-mode 0)))

(provide 'pg-completion)
