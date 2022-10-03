;;; pg-editing.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq tab-width 4                     ;; Set tab length
      custom-buffer-indent 2
      display-line-numbers-type 'relative)
(setq-default indent-tabs-mode nil    ;; Disable tab caracter
              fill-column 80)         ;; 80 caracter column indicator
(show-paren-mode 1)                   ;; Enable delimiters matching
(save-place-mode 1)                   ;; Remembers last cursor placement in file
(column-number-mode)                  ;; Show column numbers
(mouse-avoidance-mode 'banish)        ;; No mouse allowed
(global-display-line-numbers-mode 1)  ;; Show line numbers
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'compilation-filter-hook
          #'(lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(dolist (mode '(org-mode-hook         ;; Disable line numbers for some modes
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                coming-mode-hook
                gfm-view-mode-hook
                compilation-mode-hook
                dashboard-mode-hook
                eshell-mode-hook
                sql-interactive-mode-hook
                pdf-view-mode-hook
                telega-root-mode-hook
                telega-chat-mode
                telega-image-mode
                sokoban-mode-hook
                doc-view-mode-hook
                mu4e-main-mode-hook
                Man-mode-hook
                simple-mpc-mode-hook
                treemacs-mode-hook
                vterm-mode-hook
                geiser-repl-mode-hook
                slack-mode-hook
                shell-mode-hook))
  (add-hook mode #'(lambda() (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 120)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'regular)

(set-face-attribute 'italic nil :slant 'italic)

(straight-use-package '(ligature :type git
                                 :host github
                                 :repo "mickeynp/ligature.el"))
(require 'ligature)
(with-eval-after-load 'ligature
  (ligature-set-ligatures 't
                          '("++" "--" "/=" "&&" "||" "||=" "->" "=>" "::" "__"
                            "==" "===" "!=" "=/=" "!==" "<=" ">=" "<=>" "/*"
                            "*/" "//" "///" "\\n" "\\\\" "<<" "<<<" "<<=" ">>"
                            ">>>" ">>=" "|=" "^=" "**" "?." "</" "<!--" "</>"
                            "-->" "/>" "www" "##" "###" "####" "#####" "######"
                            "--" "---" "----" "-----" "------" "====" "====="
                            "======" "[]" "<>" "<~>" "??" ".." "..." "=~" "!~"
                            ":=" "..<" "!!" ":::" "=!=" "=:=" "<:<" "..=" "::<"
                            "#{" "#(" "#_" "#_(" "#?" "#:" ".-" ";;" "~@" "<-"
                            "#{}" "|>" "=>>" "=<<" ">=>" "<=<" "=>=" "=<=" "<$"
                            "<$>" "$>" "<+" "<+>" "+>" "<*" "<*>" "*>" "<|>"
                            ".=" "#=" "+++" "***" ":>:" ":<:" "<|||" "<||" "<|"
                            "||>" "|||>" "[|" "|]" "~-" "~~" "%%" "/\\" "\\/"
                            "-|" "_|" "_|_" "|-" "||-" ":>" ":<" ">:" "<:" "::>"
                            "<::" ">::" "{|" "|}" "#[" "]#" "::=" "#!" "#="
                            "->>" ">-" ">>-" "->-" "->>-" "=>>=" ">>->" ">>=>"
                            "|->" "|=>" "~>" "~~>" "//=>" "<<-" "-<" "-<<" "-||"
                            "-<-" "-<<-" "=<" "=|" "=||" "=<<=" "<-<<" "<=<<"
                            "<-|" "<=|" "<~" "<~~" "<=//" "<->" "<<=>>" "|-|-|"
                            "|=|=|" "/=/"))
  (global-ligature-mode))

(straight-use-package 'rainbow-delimiters)
(unless (fboundp 'rainbow-delimiters-mode)
  (autoload #'rainbow-delimiters-mode "rainbow-delimiters" nil t))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'abbrev)
(with-eval-after-load 'abbrev
  (if (fboundp 'diminish)
      (diminish #'abbrev-mode)
    (with-eval-after-load 'diminish
      (diminish #'abbrev-mode))))

(straight-use-package 'highlight-indent-guides)
(unless (fboundp 'highlight-indent-guides-mode)
  (autoload #'highlight-indent-guides-mode "rainbow-delimiters" nil t))
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(with-eval-after-load 'highlight-indent-guides
  (pg/customize-set-variables
   '((highlight-indent-guides-responsive . stack)
     (highlight-indent-guides-method . character))))

(straight-use-package 'smartparens)
(require 'smartparens)
(with-eval-after-load 'smartparens
  (smartparens-global-mode)
  (if (boundp 'diminish)
      (diminish #'smartparens-global-mode)
    (with-eval-after-load 'diminish
      (diminish #'smartparens-global-mode))))

(straight-use-package 'outshine)
(unless (fboundp 'outshine-mode)
  (autoload #'outshine-mode "outshine" nil t))
(add-hook 'prog-mode-hook #'outshine-mode)

(defun pg/selectric-type-sound ()
  "Make the sound of the printing element hitting the paper."
  (progn
    (selectric-make-sound (format "%sselectric-move.wav" selectric-files-path))
    (unless (minibufferp)
      (if (= (current-column) (current-fill-column))
          (selectric-make-sound (format "%sping.wav" selectric-files-path))))))

(straight-use-package 'selectric-mode)
(fset #'selectric-type-sound #'pg/selectric-type-sound)
(unless (fboundp 'selectric-mode)
  (autoload #'selectric-mode "selectric-mode" nil t))

(straight-use-package 'rainbow-mode)
(unless (fboundp 'rainbow-mode)
  (autoload #'rainbow-mode "rainbow-mode" nil t))
(dolist (mode '(org-mode-hook
                emacs-lorg-mode-hook
                org-mode-hook
                typescrorg-mode-hook
                org-mode-hook
                scss-mode-hook
                less-css-mode-hook))
  (add-hook mode #'rainbow-mode))
(with-eval-after-load 'rainbow-mode
  (if (boundp 'diminish)
      (diminish #'rainbow-mode)
    (with-eval-after-load 'diminish
      (diminish #'rainbow-mode))))

(straight-use-package 'emojify)
(require 'emojify)
(with-eval-after-load 'emojify
  (global-emojify-mode))

(defun pg/evil-hook()
  "Configuration of some default modes."
  (dolist (mode '(messages-buffer-mode
                  dashboard-mode))
    (evil-set-initial-state mode 'normal))
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

(straight-use-package 'evil)
(customize-set-variable 'evil-want-keybinding nil)
(require 'evil)
(with-eval-after-load 'evil
  (add-hook 'evil-mode-hook #'pg/evil-hook)
  (pg/customize-set-variables
   `((evil-want-integration . t)
     (evil-want-C-u-scroll . t)
     (evil-want-C-i-jump . nil)
     (evil-want-Y-yank-to-eol . t)
     (evil-want-fine-undo . t)
     (evil-undo-system . ,#'undo-redo)))
  (unless (fboundp 'evil-normal-state)
    (autoload #'evil-normal-state "evil-states"))
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(straight-use-package 'evil-collection)
(with-eval-after-load 'evil
  (require 'evil-collection))
(with-eval-after-load 'evil-collection
  (evil-collection-init)
  (if (boundp 'diminish)
      (diminish #'evil-collection-unimpaired-mode)
    (with-eval-after-load 'diminish
      (diminish #'evil-collection-unimpaired-mode))))

(provide 'pg-editing)
