(use-modules
 (guix inferior)
 (guix channels)
 (srfi srfi-1))

(define channels
  (list
   (channel (name 'guix)
            (url "https://git.savannah.gnu.org/git/guix.git")
            (commit "36b38bb4f3f4926431ae2a6d6ae79d5ad0d3d079"))))

(define inferior
  (inferior-for-channels channels))

(packages->manifest
 (list
  (specification->package "emacs")
  ;; (specification->package "emacs-native-comp")

  ;; (specification->package "emacs-exwm")
  ;; (specification->package "emacs-desktop-environment")
  
  (first (lookup-inferior-packages inferior "emacs-guix"))
  
  (first (lookup-inferior-packages inferior "emacs-geiser"))
  
  ;; (specification->package "emacs-pinentry")
  (specification->package "pinentry-emacs")
  
  ;; (specification->package "emacs-password-store")
  
  ;; (specification->package "emacs-keycast")
  
  ;; (specification->package "emacs-marginalia")
  
  ;; (specification->package "emacs-consult")
  
  ;; (specification->package "emacs-orderless")
  
  ;; (specification->package "emacs-corfu")
  
  ;; (specification->package "emacs-vertico")
  
  ;; (specification->package "emacs-embark")
  
  ;; (specification->package "emacs-prescient")
  
  ;; (specification->package "emacs-which-key")
  
  ;; (specification->package "emacs-helm")
  
  ;; (specification->package "emacs-diminish")
  
  ;; (specification->package "emacs-all-the-icons")
  
  ;; (specification->package "emacs-doom-modeline")
  
  ;; (specification->package "emacs-dashboard") ;; old version 1.7.0, need 1.8.0
  
  ;; (specification->package "emacs-page-break-lines")
  
  ;; (specification->package "emacs-perspective")
  
  (specification->package "mu")
  (specification->package "isync")
  (specification->package "oauth2ms")
  
  ;; (specification->package "emacs-mu4e-alert")
  
  ;; (specification->package "emacs-rainbow-delimiters")
  
  ;; (specification->package "emacs-highlight-indent-guides") ;; Old version 0.8.5
  
  ;; (specification->package "emacs-smartparens")
  
  ;; (specification->package "emacs-outshine")
  
  ;; (specification->package "emacs-rainbow-mode")
  
  ;; (specification->package "emacs-emojify")
  
  ;; (specification->package "emacs-evil")
  
  ;; (specification->package "emacs-evil-collection")
  
  ;; (specification->package "emacs-helpful")
  
  ;; (specification->package "emacs-visual-fill-column")
  
  ;; (specification->package "emacs-all-the-icons-dired")
  
  ;; (specification->package "emacs-openwith")
  
  ;; (specification->package "emacs-eshell-syntax-highlighting")
  
  ;; (specification->package "emacs-esh-autosuggest")
  
  (specification->package "emacs-vterm")
  
  ;; (specification->package "emacs-projectile") ;; old version 2.5.0
  
  ;; (specification->package "emacs-magit")
  
  ;; (specification->package "emacs-git-gutter")
  ;; (specification->package "emacs-git-gutter-fringe")
  
  ;; (specification->package "emacs-forge")
  
  ;; (specification->package "emacs-lsp-mode")
  
  ;; (specification->package "emacs-lsp-ui")
  
  ;; (specification->package "emacs-lsp-treemacs")
  
  ;; (specification->package "emacs-company")
  
  ;; (specification->package "emacs-company-box")
  
  ;; (specification->package "emacs-flycheck")
  
  ;; (specification->package "emacs-dap-mode")
  
  (specification->package "plantuml")
  ;; (specification->package "emacs-plantuml-mode")
  
  ;; (specification->package "emacs-yasnippet")
  
  ;; (specification->package "emacs-yasnippet-snippets")
  
  ;; (specification->package "emacs-ccls")
  
  ;; (specification->package "emacs-sly")
  
  ;; (specification->package "emacs-docker")
  
  ;; (specification->package "emacs-dockerfile-mode")
  
  ;; (specification->package "emacs-git-modes")
  
  ;; (specification->package "emacs-haskell-mode")
  
  ;; (specification->package "emacs-lsp-java")
  
  ;; (specification->package "emacs-auctex")
  
  ;; (specification->package "emacs-company-auctex")
  
  ;; (specification->package "emacs-markdown-mode")
  
  ;; (specification->package "emacs-jupyter")
  
  ;; (specification->package "emacs-typescript-mode")
  
  ;; (specification->package "emacs-yaml-mode")
  
  ;; (specification->package "emacs-alert")
  
  ;; (specification->package "emacs-org")
  
  ;; (specification->package "emacs-org-appear")
  
  ;; (specification->package "emacs-org-bullets")
  
  ;; (specification->package "emacs-org-tree-slide")
  
  ;; (specification->package "emacs-ox-reveal")
  
  ;; (specification->package "emacs-org-msg")
  
  ;; (specification->package "emacs-org-roam")
  
  ;; (specification->package "emacs-org-fragtog")
  
  ;; (specification->package "emacs-djvu")
  
  ;; (specification->package "emacs-elfeed")
  
  (specification->package "emacs-pdf-tools")
  
  (specification->package "ledger")
  ;; (specification->package "emacs-ledger-mode")
  
  ;; (specification->package "emacs-slack") ;; Older commit
  
  ;; (specification->package "emacs-sx")
  
  (first (lookup-inferior-packages inferior "emacs-telega"))
  (first (lookup-inferior-packages inferior "emacs-telega-contrib"))
  
  ;; (specification->package "emacs-wttrin")
  
  ;; (specification->package "emacs-emms")
  
  ;; (specification->package "emacs-emms-mode-line-cycle")
  
  ;; (specification->package "emacs-general")
  
  ;; (specification->package "emacs-hydra")
  
  ))
