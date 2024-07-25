(define-module (pg home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (pg packages mail)
  #:export (home-emacs-service-type))

(define (home-emacs-profile-service config)
  (list emacs-geiser
        emacs-geiser-guile
        emacs-guix

        ;; emacs-password-store
        emacs-pinentry
        pinentry-emacs

        ;; emacs-keycast

        ;; emacs-consult
        ;; emacs-corfu
        ;; emacs-embark
        ;; emacs-helm
        ;; emacs-marginalia
        ;; emacs-orderless
        ;; emacs-prescient
        ;; emacs-vertico
        ;; emacs-which-key

        ;; emacs-dashboard
        ;; emacs-diminish
        ;; emacs-doom-modeline
        ;; emacs-page-break-lines
        ;; emacs-nerd-icons

        ;; emacs-bufler
        ;; emacs-perspective

        isync
        ;; emacs-mu4e-alert
        mu-stable

        ;; emacs-editorconfig
        ;; emacs-emojify
        ;; emacs-evil
        ;; emacs-evil-collection
        ;; emacs-highlight-indent-guides
        ;; emacs-ligature
        ;; emacs-outshine
        ;; emacs-rainbow-delimiters
        ;; emacs-rainbow-mode
        ;; emacs-smartparens

        ;; emacs-helpful
        ;; emacs-visual-fill-column

        ;; emacs-dired-hide-dotfiles
        ;; emacs-dired-single
        ;; emacs-openwith
        ;; emacs-nerd-icons-dired
        ;; emacs-subed

        ;; emacs-esh-autosuggest
        ;; emacs-eshell-git-prompt
        ;; emacs-eshell-syntax-highlighting
        emacs-vterm

        ;; emacs-forge
        ;; emacs-git-gutter
        ;; emacs-git-gutter-fringe
        ;; emacs-magit
        ;; emacs-projectile

        ;; emacs-comment-dwim-2
        ;; emacs-company
        ;; emacs-company-box
        ;; emacs-company-prescient
        ;; emacs-dap-mode
        ;; emacs-flycheck
        ;; emacs-lsp-mode
        ;; emacs-lsp-treemacs
        ;; emacs-lsp-ui
        ;; emacs-plantuml-mode
        ;; emacs-yasnippet
        ;; emacs-yasnippet-snippets
        ;; plantuml

        ;; emacs-ada-mode

        ;; emacs-alloy-mode

        ;; emacs-company-arduino
        ;; emacs-arduino-mode

        ;; emacs-company-c-headers
        ;; emacs-ccls
        ;; emacs-company-irony
        ;; emacs-irony-mode

        ;; emacs-clojure-mode

        ;; emacs-sly

        ;; emacs-docker

        ;; emacs-erlang

        ;; emacs-gdscript-mode

        ;; emacs-git-modes

        ;; emacs-groovy-modes

        ;; emacs-haskell-mode

        ;; emacs-web-mode

        ;; emacs-i3wm-config-mode

        ;; emacs-lsp-java

        ;; emacs-json-mode

        ;; emacs-auctex
        ;; emacs-company-auctex

        ;; emacs-markdown-mode

        ;; emacs-mips-mode

        ;; emacs-php-mode

        ;; emacs-conda
        ;; emacs-jupyter
        ;; emacs-lsp-pyright

        ;; emacs-sbt-mode
        ;; emacs-scala-mode
        ;; emacs-lsp-metals

        ;; emacs-sql-indent

        ;; emacs-typescript-mode

        ;; emacs-yaml-mode

        ;; emacs-alert

        ;; emacs-org
        ;; emacs-org-appear
        ;; emacs-org-contacts
        ;; emacs-org-fragtog
        ;; emacs-org-modern
        ;; emacs-org-msg
        ;; emacs-org-notify
        ;; emacs-org-roam ;; Error with emacs-compat
        ;; emacs-org-tree-slide
        ;; emacs-ox-reveal

        ;; emacs-djvu
        ;; emacs-elfeed
        emacs-pdf-tools
        ;; emacs-nov-el

        emacs-ledger-mode
        ledger

        ;; emacs-ement
        ;; emacs-slack
        ;; emacs-sx

        ;; emacs-wttrin

        ;; emacs-general
        ;; emacs-hydra
	))

(define (home-emacs-environment-variables-service config)
  '(("EMACSLOADPATH" . "$HOME/.guix-home/profile/share/emacs/site-lisp:$EMACSLOADPATH")))

(define (home-emacs-shepherd-service config)
  (shepherd-service
   (documentation "Runs `emacs' as a daemon")
   (provision '(emacs))
   (start #~(make-forkexec-constructor
             (list #$(file-append emacs-no-x-toolkit "/bin/emacs")
                   "--fg-daemon")))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-emacs-shepherd-services config)
  (list (home-emacs-shepherd-service config)))

(define home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Various emacs packages")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-emacs-profile-service)
		       (service-extension home-environment-variables-service-type
					  home-emacs-environment-variables-service)
                       (service-extension home-shepherd-service-type
                                          home-emacs-shepherd-services)))
                (default-value #f)))
