(setq gc-cons-threshold (* 50 1000 1000)) ; Sets garbage collection threshold high enough

(server-start)

(setq pg/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(setq inhibit-startup-message t)                                   ; Disable startup message
(setq scroll-conservatively 1000)                                  ; Slow scrolling
(unless pg/is-termux
  (scroll-bar-mode 0)                                                ; Disable visible scrollbar
  (tool-bar-mode 0)                                                  ; Disable toolbar
  (tooltip-mode 0))

(menu-bar-mode 0)                                                  ; Disable menu bar
(setq split-width-threshold 185)                                   ; Width for splitting
(global-set-key (kbd "M-<tab>") 'other-window)                     ; Bind alt tab to buffer switching

;; Set frame transparency
(unless pg/is-termux
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(require 'iso-transl)
(define-key global-map (kbd "<Multi_key>") iso-transl-ctl-x-8-map) ; Bind compose key in case emacs captures it

(setq tab-width 4)                    ; Set tab length
(setq custom-buffer-indent 2)
(setq-default indent-tabs-mode nil)   ; Disable tab caracter
(show-paren-mode 1)                   ; Enable delimiters matching
(save-place-mode 1)                   ; Remembers last cursor placement in file
(column-number-mode)                  ; Show column numbers
(mouse-avoidance-mode 'banish)        ; No mouse allowed
(global-display-line-numbers-mode 1)  ; Show line numbers
(setq display-line-numbers-type 'relative)
(setq-default fill-column 80)         ; 80 caracter column indicator
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(dolist (mode '(org-mode-hook         ; Disable line numbers for some modes
                term-mode-hook
                coming-mode-hook
                gfm-view-mode-hook
                compilation-mode-hook
                eshell-mode-hook
                sql-interactive-mode-hook
                pdf-view-mode-hook
                sokoban-mode-hook
                doc-view-mode-hook
                mu4e-main-mode-hook
                Man-mode-hook
                simple-mpc-mode-hook
                treemacs-mode-hook
                vterm-mode-hook
                slack-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina")
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :weight 'regular)

(set-face-attribute 'italic nil
                    :slant 'italic 
                    :underline nil)

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; (require 'package) ; Initialize package sources

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))
;; (package-initialize)
;; (setq package-enable-at-startup nil)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package) 
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;(setq use-package-verbose t) For optimizing performance

(use-package auth-source
  :straight nil
  :custom
  (auth-sources '("~/.authinfo.gpg")))

(unless pg/is-termux
  (use-package pinentry
    :custom
    (epg-pinentry-mode 'loopback)
    :config
    (pinentry-start)))

(use-package password-cache
  :straight nil
  :custom
  (password-cache-expiry (* 60 60 2)))

(defun pg/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(define-minor-mode pg/keycast-mode
  "Show current command and its key binding in the mode line (fix for use with doom-mode-line)."
  :global t
  (interactive)
  (if pg/keycast-mode
      (add-hook 'pre-command-hook 'keycast--update t)
    (remove-hook 'pre-command-hook 'keycast--update)))

(use-package keycast
  :custom
  (keycast-mode-line-format "%2s%k%c%2s")
  :config
  (fset #'keycast-mode #'pg/keycast-mode)
  (keycast-mode)
  (add-to-list 'global-mode-string '("" keycast-mode-line)))

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
  (corfu-cycle t)
  :config
  (corfu-global-mode))

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

(use-package app-launcher
  :straight '(app-launcher
              :host github
              :repo "SebastienWae/app-launcher"))

(use-package prescient
  :straight t)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1)) ; Delay before popup in seconds

(use-package helm
  :after lsp-java
  :bind
  (:map helm-map
        ("C-k" . helm-previous-line)
        ("C-j" . helm-next-line))
  :config
  (helm-mode 1))

(use-package diminish
  :straight t)

(use-package all-the-icons)

(use-package ligature
  :straight nil
  :load-path "~/Packages/ligature.el"
  :config
  ;; Enable ligatures
  (ligature-set-ligatures 't '("++" "--" "/=" "&&" "||" "||=" "->" "=>" "::" "__" "==" "===" "!=" "=/=" "!=="
                               "<=" ">=" "<=>" "/*" "*/" "//" "///" "\\n" "\\\\" "<<" "<<<" "<<=" ">>" ">>>" ">>="
                               "|=" "^=" "**" "?." "</" "<!--" "</>" "-->" "/>" "www" "##" "###" "####" "#####"
                               "######" "--" "---" "----" "-----" "------" "====" "=====" "======" "[]" "<>" "<~>"
                               "??" ".." "..." "=~" "!~" ":=" "..<" "!!" ":::" "=!=" "=:=" "<:<" "..=" "::<"
                               "#{" "#(" "#_" "#_(" "#?" "#:" ".-" ";;" "~@" "<-" "#{}" "|>" "=>>" "=<<" ">=>" "<=<"
                               "=>=" "=<=" "<$" "<$>" "$>" "<+" "<+>" "+>" "<*" "<*>" "*>" "<|>" ".=" "#=" "+++" "***"
                               ":>:" ":<:" "<|||" "<||" "<|" "||>" "|||>" "[|" "|]" "~-" "~~" "%%" "/\\" "\\/" "-|" "_|"
                               "_|_" "|-" "||-" ":>" ":<" ">:" "<:" "::>" "<::" ">::" "{|" "|}" "#[" "]#" "::="
                               "#!" "#=" "->>" ">-" ">>-" "->-" "->>-" "=>>=" ">>->" ">>=>" "|->" "|=>" "~>" "~~>" "//=>"
                               "<<-" "-<" "-<<" "-||" "-<-" "-<<-" "=<" "=|" "=||" "=<<=" "<-<<" "<=<<" "<-|" "<=|" "<~"
                               "<~~" "<=//" "<->" "<<=>>" "|-|-|" "|=|=|" "/=/"))

  (global-ligature-mode 't))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-modal-icon nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-indent-info t)
  (doom-modeline-mu4e t))

(use-package autothemer
  :config
  (load-theme 'onedark-variant t))

(defun pg/dashboard-setup-startup-hook ()
  "Setup post initialization hooks."
  (add-hook 'after-init-hook (lambda ()
                               ;; Display useful lists of items
                               (dashboard-insert-startupify-lists)))
  (add-hook 'emacs-startup-hook (lambda ()
                                  (switch-to-buffer dashboard-buffer-name)
                                  (goto-char (point-min))
                                  (redisplay)
                                  (run-hooks 'dashboard-after-initialize-hook))))

(defun pg/display-startup-time ()
  (let ((package-count 0) (time (float-time (time-subtract after-init-time before-init-time))))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-count straight--profile-cache) package-count)))
    (if (zerop package-count)
        (format "Emacs started in %.2f" time)
      (format "%d packages loaded in %.2f seconds with %d garbage collections" package-count time gcs-done))))

(use-package dashboard
  :straight t
  :custom
  (dashboard-set-file-icons t)
  (dashboard-items '((recents . 10)
                     (projects . 10)
                     (agenda . 5)))
  (dashboard-init-info #'pg/display-startup-time)

  :config
  (fset #'dashboard-setup-startup-hook #'pg/dashboard-setup-startup-hook)
  (pg/dashboard-setup-startup-hook))

(use-package page-break-lines)

(use-package bufler
  :straight t
  :after evil-collection
  :bind ("C-x C-b" . bufler)
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET")   'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "D"           'bufler-list-buffer-kill)

  (setf bufler-groups
        (bufler-defgroups

         ;; Subgroup collecting all named workspaces.
         (group (auto-workspace))

         ;; Subgroup collecting buffers in a projectile project.
         (group (auto-projectile))

         ;; Grouping browser windows
         (group
          (group-or "Browsers"
                    (name-match "Firefox" (rx bos "firefox"))))

         (group
          (group-or "Chat"
                    (name-match "Discord" (rx bos "discord"))
                    (mode-match "Slack" (rx bos "slack-"))))

         (group
          ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
          (group-or "Help/Info"
                    (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                    (mode-match "*Info*" (rx bos "info-"))))

         (group
          ;; Subgroup collecting all special buffers (i.e. ones that are not
          ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
          ;; through to other groups, so they end up grouped with their project buffers).
          (group-and "*Special*"
                     (name-match "**Special**"
                                 (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                     (lambda (buffer)
                       (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                            buffer)
                                   (funcall (mode-match "Dired" (rx bos "dired"))
                                            buffer)
                                   (funcall (auto-file) buffer))
                         "*Special*"))))

         ;; Group remaining buffers by major mode.
         (auto-mode))))

(use-package winner
  :straight nil
  :config
  (winner-mode))

(use-package tab-bar
  :custom
  (tab-bar-show 1)
  :config
  (tab-bar-mode))

(use-package perspective
  :straight t
  :bind
  ("C-x k" . persp-kill-buffer*)
  :config
  (unless (equal persp-mode t) (persp-mode)))

(unless pg/is-termux
  (use-package mu4e
    :straight '(mu :type git
                   :host github
                   :branch "release/1.6"
                   :repo "djcb/mu"
                   :files ("mu4e/*")
                   :pre-build (("./autogen.sh") ("make")))
    :commands mu4e
    ;; :load-path "/usr/local/share/emacs/site-lisp/mu4e"
    :config
    (require 'mu4e-org)
    ;; This is set to 't' to avoid mail syncing issues when using mbsync
    (setq mu4e-change-filenames-when-moving t)

    ;; Refresh mail using isync every 10 minutes
    (setq mu4e-update-interval (* 10 60))
    (setq mu4e-get-mail-command "mbsync -a")
    (setq mu4e-maildir "~/Mail")
    (setq message-send-mail-function 'smtpmail-send-it)
    (setq mu4e-compose-format-flowed t)
    (setq mu4e-compose-signature
          (concat "Philippe Gabriel - \n[[mailto:philippe.gabriel.1@umontreal.ca][philippe.gabriel.1@umontreal.ca]] | "
                  "[[mailto:pgabriel999@hotmail.com][pgabriel999@hotmail.com]]"))
    (setq mu4e-compose-signature-auto-include nil)

    (setq mu4e-contexts
          (list
           ;; Main account
           (make-mu4e-context
            :name "Main"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Main" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "pgabriel999@hotmail.com")
                    (user-full-name . "Philippe Gabriel")
                    (smtpmail-smtp-server . "smtp.office365.com")
                    (smtpmail-smtp-user . "pgabriel999@hotmail.com")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-stream-type . starttls)
                    (mu4e-drafts-folder . "/Main/Drafts")
                    (mu4e-sent-folder . "/Main/Sent")
                    (mu4e-refile-folder . "/Main/Archive")
                    (mu4e-trash-folder . "/Main/Deleted")))

           ;; University account
           (make-mu4e-context
            :name "University"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/University" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "philippe.gabriel.1@umontreal.ca")
                    (user-full-name . "Philippe Gabriel")
                    (smtpmail-smtp-server . "smtp.office365.com")
                    (smtpmail-smtp-user . "philippe.gabriel.1@umontreal.ca")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-stream-type . starttls)
                    (mu4e-drafts-folder . "/University/Drafts")
                    (mu4e-sent-folder . "/University/Sent Items")
                    (mu4e-refile-folder . "/University/Archive")
                    (mu4e-trash-folder . "/University/Deleted Items")))))

    (setq mu4e-maildir-shortcuts
          '(("/University/Inbox" . ?u)
            ("/University/Drafts" . ?d)
            ("/Main/Inbox" . ?m)
            ("/Main/Jobs" . ?j)
            ("/Main/University" . ?s)))
    (mu4e t)
    :custom
    (mu4e-context-policy 'pick-first)
    (mu4e-mu-binary (expand-file-name "mu/mu" (straight--repos-dir "mu")))
    ;; (setq mu4e-bookmarks
    ;;       '((:name "Display Name" :query "Query" :key "Key" ...)))
    ))

(unless pg/is-termux
  (straight-use-package 'mu4e-alert)
  (use-package mu4e-alert
    :after mu4e
    :custom
    (mu4e-alert-notify-repeated-mails t)
    :config
    (mu4e-alert-set-default-style 'notifications)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package abbrev
  :straight nil
  :diminish abbrev-mode)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom 
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-method 'character))

(use-package undo-fu)

(use-package smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(with-eval-after-load 'smartparens
  (sp-with-modes
      '(smartparens-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package outshine
  :straight nil
  :hook (prog-mode . outshine-mode)
  :config
  (unbind-key "M-<up>" 'outshine-mode-map)
  (unbind-key "M-<down>" 'outshine-mode-map)
  (unbind-key "<normal-state> [ [" 'outline-mode-map)
  (unbind-key "<normal-state> ] ]" 'outline-mode-map)
  (unbind-key "C-c @ C-p" 'outline-minor-mode-map)
  (unbind-key "C-c @ C-n" 'outline-minor-mode-map)
  (unbind-key "<normal-state> C-k" 'outline-mode-map)
  (unbind-key "<normal-state> C-j" 'outline-mode-map)
  :bind (:map outline-minor-mode-map
              ("C-j" . outline-next-visible-heading)
              ("C-k" . outline-previous-visible-heading)))

(defun pg/selectric-type-sound ()
  "Make the sound of the printing element hitting the paper."
  (progn
    (selectric-make-sound (format "%sselectric-move.wav" selectric-files-path))
    (unless (minibufferp)
      (if (= (current-column) (current-fill-column))
          (selectric-make-sound (format "%sping.wav" selectric-files-path))))))

(use-package selectric-mode
  :config
  (fset #'selectric-type-sound #'pg/selectric-type-sound))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((org-mode
          emacs-lisp-mode
          web-mode
          typescript-mode
          css-mode
          scss-mode
          less-css-mode) . rainbow-mode))

(use-package emojify)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package dired
  :straight nil
  :after evil-collection
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)) ; Open dired at current directory
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :straight t
  :after dired
  :commands (dired dired-jump))

(unless pg/is-termux
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package dired-hide-dotfiles
  :after evil-collection
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(defun pg/eshell-git-prompt-multiline ()
  "Eshell Git prompt inspired by spaceship-prompt."
  (let (separator hr dir git git-dirty time sign command)
    (setq separator (with-face " | " 'eshell-git-prompt-multiline-secondary-face))
    (setq hr (with-face (concat "\n" (make-string (/ (window-total-width) 2) ?─) "\n") 'eshell-git-prompt-multiline-secondary-face))
    (setq dir
          (concat
           (with-face " " 'eshell-git-prompt-directory-face)
           (concat  (abbreviate-file-name (eshell/pwd)))))
    (setq git
          (concat (with-face "⎇" 'eshell-git-prompt-exit-success-face)
                  (concat (eshell-git-prompt--branch-name))))
    (setq git-dirty
          (when (eshell-git-prompt--branch-name)
            (if (eshell-git-prompt--collect-status)
                (with-face " ✎" 'eshell-git-prompt-modified-face)
              (with-face " ✔" 'eshell-git-prompt-exit-success-face))))
    (setq time (with-face (format-time-string "%I:%M:%S %p") 'eshell-git-prompt-multiline-secondary-face))
    (setq sign
          (if (= (user-uid) 0)
              (with-face "\n#" 'eshell-git-prompt-multiline-sign-face)
            (with-face "\nλ" 'eshell-git-prompt-multiline-sign-face)))
    (setq command (with-face " " 'eshell-git-prompt-multiline-command-face))

    ;; Build prompt
    (concat hr dir separator git git-dirty separator time sign command)))

(use-package eshell-git-prompt
  :after eshell
  :config
  (fset #'eshell-git-prompt-multiline #'pg/eshell-git-prompt-multiline))

(defun pg/config-path ()
  (let ((paths '("/home/phil-gab99/miniconda3/bin"
                 "/home/phil-gab99/miniconda3/condabin"
                 "/opt/pulsesecure/bin"
                 "/home/phil-gab99/bin"
                 "/home/phil-gab99/Visual_Paradigm_16.3/bin"
                 "/home/phil-gab99/yakindu-sctpro"
                 "/home/phil-gab99/PIPEv4.3.0"
                 "/home/phil-gab99/.dotnet"
                 "/home/phil-gab99/.cabal/bin"
                 "/home/phil-gab99/.ghcup/bi")))
    (dolist (path paths)
      (add-to-list 'exec-path path))))

(defun pg/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (local-unset-key (kbd "M-<tab>"))
  (corfu-mode)

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(require 'em-tramp)
(use-package eshell
  :straight nil
  :hook (eshell-first-time-mode . pg/configure-eshell)
  :custom
  (eshell-prefer-lisp-functions t)
  :config
  (pg/config-path)
  (eshell-git-prompt-use-theme 'multiline))

(use-package vterm)

(use-package projectile
  :diminish projectile-mode
  :hook (lsp-mode . projectile-mode)
  :custom ((projectile-completion-system 'vertico))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(bind-keys*
 :map prog-mode-map
 ("C-p c" . projectile-run-project)
 ("C-p b" . projectile-compile-project))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :config
  (unbind-key "M-<tab>" 'magit-mode-map)
  (unbind-key "M-<tab>" 'magit-section-mode-map)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :straight git-gutter-fringe
  :diminish git-gutter-mode
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :custom
  (git-gutter:added-sign nil)
  (git-gutter:modified-sign nil)
  (git-gutter:deleted-sign nil)
  (git-gutter-fr:side 'right-fringe)
  :config
  (unless pg/is-termux
    (require 'git-gutter-fringe)
    (set-face-foreground 'git-gutter-fr:added "LightGreen")
    (fringe-helper-define 'git-gutter-fr:added nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
    (fringe-helper-define 'git-gutter-fr:modified nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"))

  ;; These characters are used in terminal mode
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package forge
  :after magit)

(defun pg/lsp-mode-setup () ; Displays structure of cursor position for all buffers
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(straight-use-package 'lsp-mode)
(require 'lsp-completion)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . pg/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-completion-provider :none))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t))

(use-package lsp-treemacs
  :after lsp)

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
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
  :after company
  :config
  (company-prescient-mode 1))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package dap-mode
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
  :custom
  (plantuml-indent-level 4)
  (plantuml-jar-path "~/bin/plantuml.jar")
  (plantuml-default-exec-mode 'jar))

(use-package alloy-mode
  :straight nil
  :hook (alloy-mode . (lambda () (setq indent-tabs-mode nil)))
  :load-path "~/.emacs.d/extrapkgs/alloy-mode"
  :custom
  (alloy-basic-offset 4))

(require 'lsp-clangd)
(use-package cc-mode
  :straight nil
  :config
  (setq c-basic-offset 4)
  :custom
  (lsp-clangd-binary-path "~/.emacs.d/lsp-servers/clangd_13.0.0/bin/clangd")
  (lsp-clangd-version "13.0.0")
  (company-clang-executable "/usr/lib/clang")
  :hook ((c-mode c++-mode objc-mode) . lsp-deferred))

(use-package company-c-headers
  :after (cc-mode company)
  :config
  (add-to-list 'company-backends '(company-c-headers :with company-yasnippet)))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

(use-package lsp-css
  :straight nil
  :hook ((css-mode less-css-mode scss-mode) . lsp-deferred))

(use-package dockerfile-mode)

(use-package git-modes)

(use-package groovy-mode
  :straight '(groovy-emacs-modes :type git
                                 :host github
                                 :repo "Groovy-Emacs-Modes/groovy-emacs-modes"))

(use-package haskell-mode
  :hook ((haskell-mode haskell-literate-mode) . lsp-deferred))

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-8.10.6"))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :bind
  (:map lsp-mode-map
        ("C-<return>" . lsp-execute-code-action))
  :config
  (require 'dap-java)
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-java-configuration-runtimes '[( :name "JavaSE-17"
                                       :path "/usr/lib/jvm/java-17-openjdk-amd64"
                                       :default t)])
  (lsp-java-vmargs (list "-noverify" "--enable-preview"))
  (lsp-java-java-path "/usr/lib/jvm/java-17-openjdk-amd64/bin/java")
  (lsp-java-import-gradle-home "/opt/gradle/latest/bin/gradle")
  (lsp-java-import-gradle-java-home "/usr/lib/jvm/java-17-openjdk-amd64/bin/java")
  (lsp-java-server-install-dir "/home/phil-gab99/.emacs.d/lsp-servers/java-language-server/bin/"))

(defun pg/gradle-run ()
  "Execute gradle run command"
  (interactive)
  (gradle-run "run"))

(use-package gradle-mode
  :hook (java-mode . gradle-mode)
  :straight '(emacs-gradle-mode
              :host github
              :repo "jacobono/emacs-gradle-mode"))

(straight-use-package 'auctex)
(require 'tex-site)
(use-package tex
  :straight auctex
  :config
  (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (put 'TeX-mode 'derived-mode-parent 'prog-mode)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t))

(use-package company-auctex
  :after (auctex company)
  :config
  (add-to-list 'company-backends '(company-auctex :with company-yasnippet)))

(defvar lmc-java-mode-hook nil)

;; (add-to-list 'auto-mode-alist '("\\.lmc\\'" . lmc-java-mode))

(defconst lmc-java-font-lock-defaults
  (list
   '("#.*" . font-lock-comment-face)
   '("\\<\\(ADD\\|BR[PZ]?\\|DAT\\|HLT\\|IN\\|LDA\\|OUT\\|S\\(?:TO\\|UB\\)\\)\\>" . font-lock-keyword-face)
   '("^\\w+" . font-lock-function-name-face)
   '("\\b[0-9]+\\b" . font-lock-constant-face))
  "Minimal highlighting expressions for lmc mode")

(defvar lmc-java-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# ". 1b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for lmc-mode")

(define-derived-mode lmc-java-mode prog-mode "LMC"
  "Major mode for editing lmc files"
  :syntax-table lmc-mode-syntax-table

  (set (make-local-variable 'font-lock-defaults) '(lmc-font-lock-defaults))

  (setq-local comment-start "# ")
  (setq-local comment-end "")

  (setq-local indent-tabs-mode nil))

(define-derived-mode pg/lmc-asm-mode prog-mode "LMC-Asm"
  "Major mode to edit LMC assembly code."
  :syntax-table emacs-lisp-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(lmc-asm-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       #'lmc-asm-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'imenu-generic-expression)
       lmc-asm-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) lmc-asm-outline-regexp)
  (add-hook 'completion-at-point-functions #'lmc-asm-completion nil t)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *"))

(use-package lmc
  :config
  (fset #'lmc-asm-mode #'pg/lmc-asm-mode))

(use-package markdown-mode
  :straight nil
  :hook (gfm-view-mode . (lambda () (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                                                       (markdown-code-face (:height 1.5) fixed-pitch))))))

(use-package mips-mode
  :mode "\\.asm\\'"
  :custom
  (mips-tab-width 4))

(use-package nusmv-mode
  :straight nil
  :load-path "~/.emacs.d/extrapkgs/nusmv-mode"
  :mode "\\.smv\\'"
  :bind*
  (:map nusmv-mode-map
        ("C-c C-c" . nusmv-run))
  (:map nusmv-m4-mode-map
        ("C-c C-c" . nusmv-run))
  :custom
  (nusmv-indent 4)
  :config
  (menu-bar-mode 0)
  (add-hook 'nusmv-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (put 'nusmv-mode 'derived-mode-parent 'prog-mode))

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  ;;(python-shell-interpreter "python3")
  ;;(dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :custom
  (lsp-python-ms-executable
   "~/.emacs.d/lsp-servers/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp-deferred))))

(use-package jupyter)

(use-package z3-mode)

(use-package sql
  :hook (sql-mode . lsp-deferred)
  :config
  (add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))
  :custom
  ;; (sql-postgres-login-params '((user :default "phil-gab99")
  ;;                              (database :default "phil-gab99")
  ;;                              (server :default "localhost")
  ;;                              (port :default 5432)))

  (sql-connection-alist
   '((main (sql-product 'postgres)
           (sql-port 5432)
           (sql-server "localhost")
           (sql-user "phil-gab99")
           (sql-password (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432))
           (sql-database "phil-gab99"))
     (school (sql-product 'postgres)
             (sql-port 5432)
             (sql-server "localhost")
             (sql-user "phil-gab99")
             (sql-password (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432))
             (sql-database "ift2935"))))

  (lsp-sqls-server "~/go/bin/sqls")
  (setq lsp-sqls-connections
   (list
    (list
     (cl-pairlis '(driver dataSourceName)
                 (list '("postgresql") (concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                        (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432)
                                        " dbname=phil-gab99 sslmode=disable")))
     (cl-pairlis '(driver dataSourceName)
                 (list '("postgresql") (concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                        (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432)
                                        " dbname=ift2935 sslmode=disable")))))))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode)
  :config
  (setq-default sqlind-basic-offset 4))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (require 'dap-node)
  (dap-node-setup))

(flycheck-define-checker vhdl-tool
  "A VHDL syntax checker, type checker and linter using VHDL-Tool."
  :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source)
  :standard-input t
  :modes (vhdl-mode)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
   (error line-start (file-name) ":" line ":" column ":e:" (message) line-end)))

(use-package vhdl-tools
  :hook (vhdl-mode . lsp-deferred)
  :custom
  (lsp-vhdl-server-path "~/.emacs.d/lsp-servers/vhdl-tool")
  :config
  (add-to-list 'flycheck-checkers 'vhdl-tool))

(use-package yaml-mode)

(use-package comment-dwim-2
  :bind
  ("M-/" . comment-dwim-2)
  (:map org-mode-map
        ("M-/" . org-comment-dwim-2)))

(use-package yasnippet
  :diminish yas-minor-mode
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1)
  (add-hook 'yas-minor-mode-hook (lambda ()
                                   (yas-activate-extra-mode 'fundamental-mode))))

(use-package yasnippet-snippets
  :after yasnippet
  :straight t)

(use-package alert
  :custom
  (alert-default-style 'notifications))

(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

;; Insert a file and convert it to an org table
(defun org-csv-to-table (beg end)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (mark) (point)))
  (org-table-convert-region beg end ","))

;; Function for defining some behaviours for the major org-mode
(defun pg/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  (setq evil-auto-indent nil))

(use-package org
  ;;:pin org
  ;;:straight org-plus-contrib
  :commands (org-capture org-agenda)
  :hook (org-mode . pg/org-mode-setup)
  :config
  (set-face-attribute 'org-ellipsis nil :underline nil)
  (setq org-ellipsis " ▾")
  (unless pg/is-termux
    (setq org-agenda-files ; Files considered by org-agenda
          '("~/Documents/Org/Agenda/"
            "~/Documents/Org/Recurrent/")))
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 7)
  (setq org-todo-keywords ; Defines a new sequence for TODOs, can add more sequences
        '((sequence "TODO(t)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w)" "HOLD(h)" "|"
                    "COMPLETED(c)" "CANC(k)")))

  (unless pg/is-termux
    (setq org-agenda-custom-commands ; Custom org-agenda commands
          '(("d" "Dashboard"
             ((agenda "" ((org-deadline-warning-days 7)))
              (todo "TODO"
                    ((org-agenda-overriding-header "Tasks")))
              (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Tasks")))))

            ("Z" "TODOs"
             ((todo "TODO"
                    ((org-agenda-overriding-header "Todos")))))

            ("m" "Misc" tags-todo "other")

            ("s" "Schedule" agenda ""
             ((org-agenda-files '("~/Documents/Org/Agenda/Schedule-S5.org")))
             ("~/Documents/Schedule-S5.pdf"))

            ("w" "Work Status"
             ((todo "WAIT"
                    ((org-agenda-overriding-header "Waiting")
                     (org-agenda-files org-agenda-files)))
              (todo "REVIEW"
                    ((org-agenda-overriding-header "In Review")
                     (org-agenda-files org-agenda-files)))
              (todo "HOLD"
                    ((org-agenda-overriding-header "On Hold")
                     (org-agenda-todo-list-sublevels nil)
                     (org-agenda-files org-agenda-files)))
              (todo "ACTIVE"
                    ((org-agenda-overriding-header "Active")
                     (org-agenda-files org-agenda-files)))
              (todo "COMPLETED"
                    ((org-agenda-overriding-header "Completed")
                     (org-agenda-files org-agenda-files)))
              (todo "CANC"
                    ((org-agenda-overriding-header "Cancelled")
                     (org-agenda-files org-agenda-files))))))))

  (unless pg/is-termux
    (setq org-capture-templates
          `(("t" "Tasks / Projects")
            ("tt" "Task" entry (file+olp "~/Documents/Org/Agenda/Tasks.org" "Active")
             "* TODO %?\n  DEADLINE: %U\n  %a\n  %i" :empty-lines 1)

            ("j" "Meetings")
            ("jm" "Meeting" entry (file+olp "~/Documents/Org/Agenda/Tasks.org" "Waiting")
             "* TODO %? \n SCHEDULED: %U\n" :empty-lines 1)

            ("m" "Email Workflow")
            ("mr" "Follow Up" entry (file+olp "~/Documents/Org/Agenda/Mail.org" "Follow up")
             "* TODO %a\nDEADLINE: %U%?\n %i" :empty-lines 1))))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (setq org-agenda-exporter-settings
        '((ps-left-header (list 'org-agenda-write-buffer-name))
          (ps-right-header
           (list "/pagenumberstring load"
                 (lambda () (format-time-string "%d/%m/%Y"))))
          (ps-font-size '(12 . 11))       ; Lanscape . Portrait
          (ps-top-margin 55)
          (ps-left-margin 35)
          (ps-right-margin 30)))
  (unless pg/is-termux
    (setq org-plantuml-jar-path "~/bin/plantuml.jar"))
  :custom

  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame))))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun pg/diminish-all ()
  (diminish 'which-key-mode)
  (diminish 'org-indent-mode)
  (diminish 'auto-revert-mode)
  (diminish 'buffer-face-mode)
  (diminish 'visual-line-mode))

(defun pg/presentation-setup ()
  (org-display-inline-images)
  (pg/diminish-all)
  (setq-local doom-modeline-minor-modes t)
  (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  (setq-local face-remapping-alist '((default (:height 1.25) default)
                                     (header-line (:height 4.5) variable-pitch)
                                     (variable-pitch (:height 1.25) variable-pitch)
                                     (org-table (:height 1.5) org-table)
                                     (org-verbatim (:height 1.5) org-verbatim)
                                     (org-code (:height 1.5) org-code)
                                     (org-block (:height 1.5) org-block)))
  (variable-pitch-mode 1))

(defun pg/presentation-end ()
  (variable-pitch-mode 0)
  (setq-local doom-modeline-minor-modes nil)
  (setq-local org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (org-latex-preview)
  (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package org-tree-slide
  :hook (((org-tree-slide-before-move-next org-tree-slide-before-move-previous) . org-latex-preview)
         (org-tree-slide-play . pg/presentation-setup)
         (org-tree-slide-stop . pg/presentation-end))
  :after org
  :bind*
  (:map org-tree-slide-mode-map
        ("C-j" . org-tree-slide-move-next-tree)
        ("C-k" . org-tree-slide-move-previous-tree))
  :config
  ;; (unbind-key "<normal-state> C-j" 'org-mode-map)
  ;; (unbind-key "<normal-state> C-k" 'org-mode-map)
  ;; (unbind-key "C->" 'org-tree-slide-mode-map)
  ;; (unbind-key "C-<" 'org-tree-slide-mode-map)
  :custom
  (org-tree-slide-activate-message "Presentation started")
  (org-tree-slide-deactivate-message "Presentation ended")
  (org-tree-slide-breadcrumbs " > ")
  (org-tree-slide-header t)
  (org-image-actual-width nil))

(use-package ox-reveal
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(straight-use-package 'org-notify)
(use-package org-notify
  :straight nil
  :after org
  :custom
  (user-mail-address "philippe.gabriel.1@umontreal.ca")
  :config
  (org-notify-start)
  (setq org-notify-map nil)
  (org-notify-add 'default '(:time "1w" :actions -notify/window :period "1h" :duration 5))
  (org-notify-add 'meeting '(:time "1w" :actions -email :period "1d")))

;; (org-notify-add 'appt
;;                 '(:time "-1s" :period "20s" :duration 10 :actions (-message -ding))
;;                 '(:time "15m" :period "2m" :duration 100 :actions -notify)
;;                 '(:time "2h" :period "5m" :actions -message)
;;                 '(:time "3d" :actions -email))
;;

(use-package org-mime
  :straight t
  :after org-msg)

(setq mail-user-agent 'mu4e-user-agent)
(use-package org-msg
  :straight t
  :after mu4e
  :custom
  (org-msg-options "html-postamble:nil toc:nil author:nil num:nil \\n:t")
  (org-msg-startup "indent inlineimages hidestars")
  (org-msg-greeting-fmt "\nBonjour/Hi%s,\n\n")
  ;; (org-msg-recipient-names '(("user@domain.com" . "Name")))
  (org-msg-greeting-name-limit 3)
  (org-msg-default-alternatives '((new . (text utf-8 html org))
                                  (reply-to-html . (text org html))
                                  (reply-to-text . (text org))))
  (org-message-convert-citation t)
  (org-msg-signature (concat "\n\nCordialement/Regards,\n\n*--*\n" mu4e-compose-signature))
  :config
  (org-msg-mode))

(unless pg/is-termux
  (use-package org-roam
    :straight t
    :custom
    (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-directory "~/Documents/Notes")
    (org-roam-capture-templates
     '(("d" "default" plain
        "%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("1" "databases" plain
        "%?"
        :if-new (file+head "IFT-2935/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ift2935-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("2" "operating system" plain
        "%?"
        :if-new (file+head "IFT-2245/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ift2245-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("3" "software analysis" plain
        "%?"
        :if-new (file+head "IFT-6755/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ift6755-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("4" "logic 2" plain
        "%?"
        :if-new (file+head "PHI-2005/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: phi2005-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("5" "demo2015" plain
        "%?"
        :if-new (file+head "Demo/IFT-2015/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: demo2015-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("6" "demo1215" plain
        "%?"
        :if-new (file+head "Demo/IFT-1215/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: demo1215-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)
       ("7" "demo1227" plain
        "%?"
        :if-new (file+head "Demo/IFT-1227/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: demo1227-${title}\n#+STARTUP: latexpreview inlineimages\n#+date: %U\n")
        :unnarrowed t)))

    :config
    (org-roam-setup)))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; Turns soft wrap on
(defun pg/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook ((org-mode gfm-view-mode) . pg/org-mode-visual-fill))

(font-lock-add-keywords 'org-mode ; Replace '-' with bullets
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region
                                         (match-beginning 1) (match-end 1) "•"))))))

(require 'org-indent) ; Changes some org structures to fixed pitch
(set-face-attribute 'org-block nil :foreground nil :background "gray5" :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :foreground "orange" :inherit 'fixed-pitch)
(set-face-attribute 'org-verbatim nil :foreground "green" :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :foreground "thistle3" :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(dolist (face '((org-level-1 . 1.2) ; Sets font for variable-pitch text
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))

(with-eval-after-load 'org ; Defer the body code until org is loaded
  (org-babel-do-load-languages ; Loads languages to be executed by org-babel
   'org-babel-load-languages '((emacs-lisp . t)
                               (java . t)
                               (shell . t)
                               (plantuml . t)
                               ;; (jupyter . t)
                               (python . t)))

  (setq org-confirm-babel-evaluate nil)

  (require 'org-tempo) ; Allows defined snippets to expand into appropriate code blocks
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("java" . "src java"))
  (add-to-list 'org-structure-template-alist '("als" . "src alloy"))
  (add-to-list 'org-structure-template-alist '("puml" . "src plantuml"))
  (add-to-list 'org-structure-template-alist '("vhd" . "src vhdl"))
  (add-to-list 'org-structure-template-alist '("asm" . "src mips"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("smv" . "src smv"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))

  (add-to-list 'org-src-lang-modes '("als" . alloy))
  (add-to-list 'org-src-lang-modes '("smv" . nusmv))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(defun pg/org-babel-tangle-config () ; Automatic tangle of emacs config file
  ;; (when (string-equal (file-name-directory (buffer-file-name))
  ;;                     (expand-file-name "~/.emacs.d/"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'after-save-hook #'pg/org-babel-tangle-config)))

(defun pg/start-timer ()
  (interactive)
  (setq org-clock-sound "~/Misc/ding.wav")
  (pg/study-timer))

(defun pg/start-with-break-timer () ;; For Minyi
  (interactive)
  (setq org-clock-sound "~/Misc/ding.wav")
  (pg/break-timer))

(defun pg/stop-timer ()
  (interactive)
  (setq org-clock-sound nil)
  (remove-hook 'org-timer-done-hook #'pg/study-timer)
  (remove-hook 'org-timer-done-hook #'pg/break-timer)
  (org-timer-stop))

(defun pg/study-timer ()
  (add-hook 'org-timer-done-hook #'pg/break-timer)
  (remove-hook 'org-timer-done-hook #'pg/study-timer)
  (setq org-timer-default-timer "1:00:00")
  (setq current-prefix-arg '(4)) ; Universal argument
  (call-interactively #'org-timer-set-timer))

(defun pg/break-timer ()
  (add-hook 'org-timer-done-hook #'pg/study-timer)
  (remove-hook 'org-timer-done-hook #'pg/break-timer)
  (setq org-timer-default-timer "15:00")
  (setq current-prefix-arg '(4)) ; Universal argument
  (call-interactively #'org-timer-set-timer))

(use-package doc-view
  :straight nil
  :mode ("\\.djvu\\'" . doc-view-mode))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-misc-print-programm "/usr/bin/lpr")
  (pdf-misc-print-programm-args '("-o sides=two-sided-long-edge")))

(use-package djvu)

(use-package ps-print
  :straight nil
  :bind
  (:map pdf-view-mode-map
        ("C-c C-p" . pdf-misc-print-document))
  :config
  (require 'pdf-misc)
  :custom ;; Printing double-sided
  (ps-lpr-switches '("-o sides=two-sided-long-edge"))
  (ps-spool-duplex t))

(unless pg/is-termux
  (use-package openwith
    :custom
    (large-file-warning-threshold nil)
    :config
    (setq openwith-associations
          (list
           (list
            (openwith-make-extension-regexp '("mpg" "mpeg" "mp4" "avi" "wmv" "mov" "flv" "ogm" "ogg" "mkv"))
            "mpv"
            '(file))
           (list
            (openwith-make-extension-regexp '("odt"))
            "libreoffice"
            '(file))))
    (openwith-mode 1)))

(use-package dtk
  :commands dtk
  :after evil-collection
  :config
  (evil-collection-define-key 'normal 'dtk-mode-map
    (kbd "C-j") 'dtk-forward-verse
    (kbd "C-k") 'dtk-backward-verse
    (kbd "C-f") 'dtk-forward-chapter
    (kbd "C-b") 'dtk-backward-chapter
    "q" 'dtk-quit
    "c" 'dtk-clear-dtk-buffer
    "s" 'dtk-search)
  :hook
  (dtk-mode . (lambda () (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)))))
  :custom
  (dtk-module "KJV")
  (dtk-module-category "Biblical Texts")
  (dtk-word-wrap t))

(use-package ledger-mode
  :straight t
  :mode "\\.dat\\'"
  :hook (ledger-mode . company-mode)
  :custom
  (ledger-reconcile-default-commodity "CAD")
  (ledger-binary-path "/usr/bin/ledger")
  (ledger-clear-whole-transaction t))

(use-package slack
  :commands slack-start
  :hook (slack-mode . company-mode)
  :config
  (slack-register-team :name "ift6755"
                       :default t
                       :token (pg/lookup-password :host "ift6755.slack.com"
                                                  :user "philippe.gabriel.1@umontreal.ca")
                       :cookie (pg/lookup-password :host "ift6755.slack.com"
                                                   :user "philippe.gabriel.1@umontreal.ca^cookie")
                       :subscribed-channels '((general questions random))
                       :modeline-enabled t)
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t))

(use-package sx
  :commands sx-search)

(defun pg/wttrin-fetch-raw-string (query)
  "Get the weather information based on your QUERY."
  (let ((url-user-agent "curl"))
    (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://wttr.in/" query)
         (lambda (status) (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

(use-package wttrin
  :commands wttrin
  :config
  (fset #'wttrin-fetch-raw-string #'pg/wttrin-fetch-raw-string)
  :custom
  (wttrin-default-cities '("montreal"))
  (wttrin-default-accept-language '("Accept-Language" . "en-US")))

(defun pg/start-mpd ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (pg/update-mpd-db)
  (fset #'evil-collection-simple-mpc-replace-main-view #'pg/evil-collection-simple-mpc-replace-main-view)
  (simple-mpc)
  (message "MPD Started!"))

(defun pg/kill-mpd ()
  "Stops playback and kill the music daemon."
  (interactive)
  (simple-mpc-quit)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))

(defun pg/update-mpd-db ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))


(defun pg/evil-collection-simple-mpc-replace-main-view ()
  "Update main view to show keys in use with evil mode."
  (interactive)
  (when (string= (buffer-name) simple-mpc-main-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert (propertize "* simple-mpc *\n\n"
                        'face 'simple-mpc-main-name)
            (propertize "   * controls\n" 'face 'simple-mpc-main-headers)
            "      * [p]lay/pause toggle\n"
            "      * [>] next track\n"
            "      * [<] previous track\n"
            "      * seek [f]orward\n"
            "      * seek [b]ackward\n"
            "      * [+] increase volume\n"
            "      * [-] decrease volume\n"
            "      * toggle [r]epeat mode\n"
            (propertize "\n   * playlist\n" 'face 'simple-mpc-main-headers)
            "      * view [c]urrent playlist\n"
            "      * [C]lear current playlist\n"
            "      * [S]huffle playlist\n"
            "      * [l]oad playlist\n"
            "      * [s]earch database\n"
            (propertize "\n   * misc\n" 'face 'simple-mpc-main-headers)
            "      * [q]uit")))

(use-package simple-mpc
  :commands simple-mpc
  :bind
  (:map simple-mpc-mode-map
        ("<normal-state> r" . simple-mpc-toggle-repeat))
  (:map simple-mpc-mode-map
        ("r" . simple-mpc-toggle-repeat))
  :custom
  (simple-mpc-playlist-format "#| [%id%] \t #| [%time%] \t #| [%file%]"))

(use-package sudoku
  :custom
  (sudoku-style 'unicode)
  (sudoku-level 'hard))

(use-package sokoban
  :bind
  (:map sokoban-mode-map
        ("<normal-state> h" . sokoban-move-left)
        ("<normal-state> l" . sokoban-move-right)
        ("<normal-state> k" . sokoban-move-up)
        ("<normal-state> j" . sokoban-move-down)))

;; Function for modes that should start in emacs mode
(defun pg/evil-hook()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-fine-undo t)
  (evil-mode 1)
  :hook (evil-mode . pg/evil-hook)
  :bind
  (:map evil-insert-state-map
        ("C-l" . right-word)
        ("C-h" . left-word))
  :custom
  (evil-undo-system 'undo-fu)
  :config
  (evil-set-register ?j [?f ?  ?s return escape]) ;; break at point

  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (unbind-key "C-k" 'evil-ex-completion-map)
  (unbind-key "C-k" 'evil-ex-search-keymap)
  (unbind-key "C-k" 'evil-insert-state-map)
  (unbind-key "C-k" 'evil-replace-state-map)
  (unbind-key "C-p" 'evil-normal-state-map)
  ;; Visual line motions outside visual-line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out"))

(defhydra hydra-window-scale (:timeout 5)
  "scale window"
  ("<" evil-window-decrease-width "width dec")
  (">" evil-window-increase-width "width inc")
  ("-" evil-window-decrease-height "height dec")
  ("+" evil-window-increase-height "height inc")
  ("=" balance-windows "balance"))

(defhydra hydra-x-window-scale (:timeout 5)
  "scale x window"
  ("<" (exwm-layout-shrink-window-horizontally 50) "width dec")
  (">" (exwm-layout-enlarge-window-horizontally 50) "width inc")
  ("-" (exwm-layout-shrink-window 50) "height dec")
  ("+" (exwm-layout-enlarge-window 50) "height inc")
  ("w" exwm-floating-toggle-floating "float toggle")
  ("f" exwm-layout-set-fullscreen "fullscreen"))

(defhydra hydra-window-move (:timeout 5)
  "move window"
  ("h" windmove-left "left")
  ("l" windmove-right "right")
  ("j" windmove-down "down")
  ("k" windmove-up "up"))

(defhydra hydra-window-swap (:timeout 5)
  "swap window"
  ("h" windmove-swap-states-left "left")
  ("l" windmove-swap-states-right "right")
  ("j" windmove-swap-states-down "down")
  ("k" windmove-swap-states-up "up"))

(defhydra hydra-window-change (:timeout 5)
  "change window"
  ("l" next-buffer "right")
  ("h" previous-buffer "left"))

(defhydra hydra-eyebrowse-switch (:timeout 5)
  "switch workspace"
  ("l" eyebrowse-next-window-config "next")
  ("h" eyebrowse-prev-window-config "prev"))

(defhydra hydra-perspective-switch (:timeout 5)
  "switch perspective"
  ("l" persp-next "next")
  ("h" persp-prev "prev"))

(use-package general
  :after evil
  :config
  (general-create-definer pg/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (pg/leader-keys

    ;; Chat
    "c" '(:ignore t :which-key "social")
    "cs" '(:ignore t :which-key "slack")
    "css" '(slack-start :which-key "start")
    "csc" '(slack-channel-select :which-key "channels")
    "csm" '(slack-im-select :which-key "message")
    "csr" '(slack-message-add-reaction :which-key "react")

    ;; Mail
    "m" '(:ignore t :which-key "email")
    "md" '(mu4e :which-key "dashboard")
    "mo" '(org-mime-edit-mail-in-org-mode :which-key "org edit")
    "mc" '(mu4e-compose-new :which-key "compose")


    ;; Scaling
    "s" '(:ignore t :which-key "scaling")
    "st" '(hydra-text-scale/body :which-key "scale text")
    "sw" '(hydra-window-scale/body :which-key "scale window")
    "sx" '(hydra-x-window-scale/body :which-key "scale x window")


    ;; Window navigations
    "w" '(:ignore t :which-key "window")
    "wm" '(hydra-window-move/body :which-key "move")
    "ws" '(hydra-window-swap/body :which-key "swap")
    "wc" '(hydra-window-change/body :which-key "change")

    "wu" '(winner-undo :which-key "undo config")
    "wr" '(winner-redo :which-key "redo config")

    "wp" '(:ignore t :which-key "persp")
    "wpc" '(persp-switch :which-key "create")
    "wps" '(hydra-perspective-switch/body :which-key "switch")
    "wpa" '(persp-add-buffer :which-key "add buf")
    "wpu" '(persp-set-buffer :which-key "set buf")
    "wpk" '(persp-kill :which-key "remove")

    "wt" '(:ignore t :which-key "tabs")
    "wtt" '(tab-new :which-key "create")
    "wtw" '(tab-close :which-key "close")
    "wtr" '(tab-rename :which-key "name")
    "wts" '(tab-switch :which-key "switch")
    "wtu" '(tab-undo :which-key "undo")


    ;; Project management
    "p" '(:ignore t :which-key "project")
    "ps" '(pg/eshell :which-key "eshell")
    "pg" '(:ignore t :which-key "git")
    "pgs" '(magit-status :which-key "status")
    "pgc" '(magit-clone :which-key "clone")
    "pp" '(:ignore t :which-key "projectile")
    "ppr" '(projectile-run-project :which-key "run")
    "ppc" '(projectile-compile-project :which-key "compile")
    "ppf" '(projectile-find-file :which-key "find file")


    ;; Lsp mode
    "l" '(:ignore t :which-key "lsp")

    "ld" '(:ignore t :which-key "doc")
    "ldf" '(lsp-ui-doc-focus-frame :which-key "focus frame")
    "ldu" '(lsp-ui-doc-unfocus-frame :which-key "unfocus frame")

    "li" '(:ignore t :which-key "info")
    "lit" '(treemacs :which-key "tree")
    "lio" '(lsp-treemacs-symbols :which-key "outline")
    "lie" '(lsp-treemacs-errors-list :which-key "errors")


    ;; Org mode
    "o" '(:ignore t :which-key "org")

    "ot" '(:ignore t :which-key "pomodoro")
    "ott" '(pg/start-timer :which-key "start")
    "otb" '(pg/start-with-break-timer :which-key "break")
    "ots" '(pg/stop-timer :which-key "stop")
    "otp" '(org-timer-pause-or-continue :which-key "pause")

    "os" '(org-screenshot :which-key "screenshot")
    "oc" '(org-capture :which-key "capture")
    "op" '(org-tree-slide-mode :which-key "slide")

    "ol" '(:ignore t :which-key "links")
    "olo" '(org-open-at-point :which-key "open")
    "olb" '(org-mark-ring-goto :which-key "back")

    "on" '(:ignore t :which-key "notes")
    "onl" '(org-roam-buffer-toggle :which-key "links")
    "onf" '(org-roam-node-find :which-key "find/create")
    "oni" '(org-roam-node-insert :which-key "insert/create")
    "ons" '(org-id-get-create :which-key "create subheading")))

(setq gc-cons-threshold (* 2 1000 1000))
