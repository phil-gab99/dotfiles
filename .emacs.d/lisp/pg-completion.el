(require 'savehist)
(with-eval-after-load 'savehist
  (savehist-mode))

(straight-use-package 'marginalia)
(with-eval-after-load 'vertico
  (require 'marginalia)
  (with-eval-after-load 'marginalia
    (customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy
                                                     marginalia-annotators-light
                                                     nil))
    (marginalia-mode)))

(straight-use-package 'consult)
(require 'consult)
(with-eval-after-load 'consult
  (global-set-key (kbd "C-s") #'consult-line)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history))

(straight-use-package 'orderless)
(with-eval-after-load 'vertico
  (require 'orderless)
  (with-eval-after-load 'orderless
    (customize-set-variable 'completion-styles '(orderless))
    (customize-set-variable 'completion-category-defaults nil)
    (customize-set-variable 'orderless-skip-highlighting nil)
    (customize-set-variable 'completion-category-overrides '((file (styles basic partial-completion))))))

(straight-use-package 'corfu)
(require 'corfu)
(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "C-j") #'corfu-next)
  (define-key corfu-map (kbd "C-k") #'corfu-previous)
  (customize-set-variable 'corfu-cycle t))

(straight-use-package 'vertico)
(require 'vertico)
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (customize-set-variable 'vertico-cycle t)
  (vertico-mode))

(straight-use-package 'embark)
(require 'embark)
(with-eval-after-load 'embark
  (global-set-key (kbd "C-S-a") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-d") #'embark-act)
  (customize-set-variable 'embark-confirm-act-all nil)
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(straight-use-package '(embark-consult :host github
                                       :repo "oantolin/embark"
                                       :files ("embark-consult.el")))
(require 'embark-consult)
(with-eval-after-load 'embark
  (with-eval-after-load 'consult
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))

(straight-use-package 'prescient)
(require 'prescient)

(straight-use-package 'which-key)
(require 'which-key)
(with-eval-after-load 'which-key
  (which-key-mode)
  (customize-set-variable 'which-key-idle-delay 1)
  (diminish 'which-key-mode))

(straight-use-package 'helm)
(with-eval-after-load 'lsp-java
  (require 'helm)
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-j") #'helm-next-line)
    (define-key helm-map (kbd "C-k") #'helm-previous-line)
    (if (and (eq #'java-mode major-mode) (memq #'lsp-mode local-minor-modes))
        (helm-mode 1)
      (helm-mode 0))))

(provide 'pg-completion)
