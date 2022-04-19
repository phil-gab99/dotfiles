(require 'autothemer)

(autothemer-deftheme
 onedark-variant
 "My onedark variant custom theme"
 ((((class color) (min-colors #xFFFFFF))) ;; Concerned with GUI emacs

  (onedark-variant-whitestd   "#cccccc")
  (onedark-variant-white      "#ffffff")
  (onedark-variant-gold       "#eead0e")
  (onedark-variant-black      "#282c34")
  (onedark-variant-dark       "#000000")
  (onedark-variant-doc-box    "#1a1a1a")
  (onedark-variant-cursorbg   "#4682d9")
  (onedark-variant-regionbg   "#494949")
  (onedark-variant-regionds   "#828997")
  (onedark-variant-blue       "#00bfff")
  (onedark-variant-green      "#2cff64")
  (onedark-variant-keywords   "#c678dd")
  (onedark-variant-strings    "#98c379")
  (onedark-variant-docs       "#98f979")
  (onedark-variant-modebox    "#181a1f")
  (onedark-variant-comments   "#737373")
  (onedark-variant-types      "#ecbe7b")
  (onedark-variant-vars       "#ff6347")
  (onedark-variant-link       "#61afef")
  (onedark-variant-constants  "#eead0e")
  (onedark-variant-tags       "#00eeee")
  (onedark-variant-builtin    "#00cdcd")
  (onedark-variant-preproc    "#7b447b")
  (onedark-variant-shexec     "#afeeee")
  (onedark-variant-modelnfg   "#9da5b4")
  (onedark-variant-modelnbg   "#21252b")
  (onedark-variant-modelndiff "#ff4c35")
  (onedark-variant-modelninfg "#5b6268")
  (onedark-variant-modelninbg "#21242b")
  (onedark-variant-lnnumfg    "#8f8f8f")
  (onedark-variant-lnnumbg    "#202231")
  (onedark-variant-lnsltfg    "#fefefe")
  (onedark-variant-mu4eheader "#ff83fa")
  (onedark-variant-parenmatch "#c678dd")
  (onedark-variant-error      "#ff6666")
  (onedark-variant-warn       "#f2db05")
  (onedark-variant-minibuf    "#f0fff0")
  (onedark-variant-eviln      "#818ff4")
  )

 (
  ;; General
  (cursor                   ( :background onedark-variant-cursorbg))
  (default                  ( :foreground onedark-variant-whitestd
                              :background onedark-variant-black))
  (link                     ( :foreground onedark-variant-link
                              :weight 'bold
                              :underline t))
  (region                   ( :background onedark-variant-regionbg
                              :distant-foreground onedark-variant-regionds))
  (highlight                ( :background onedark-variant-regionbg
                              :distant-foreground onedark-variant-regionds))
  (show-paren-match         ( :foreground onedark-variant-parenmatch
                              :weight 'bold))
  (sh-quoted-exec           ( :foreground onedark-variant-shexec))
  (line-number              ( :foreground onedark-variant-lnnumfg
                              :background onedark-variant-lnnumbg))
  (line-number-current-line ( :foreground onedark-variant-lnsltfg
                              :background onedark-variant-lnnumbg))
  (minibuffer-prompt        ( :foreground onedark-variant-minibuf))
  (shadow                   ( :foreground onedark-variant-white))
  (error                    ( :foreground onedark-variant-error))
  (warning                  ( :foreground onedark-variant-warn))
  (message-header-name      ( :bold t))

  ;; Persp
  (persp-selected-face ( :foreground onedark-variant-warn))

  ;; Man
  (Man-overstrike ( :foreground onedark-variant-types
                    :weight 'bold))
  (Man-underline  ( :foreground onedark-variant-keywords
                    :underline t))

  ;; Org
  (org-hide                  ( :foreground onedark-variant-black))
  (org-level-4               ( :foreground onedark-variant-green))
  (org-date                  ( :foreground onedark-variant-blue))
  (org-document-info-keyword ( :inherit '(font-lock-comment-face
                                          fixed-pitch)))
  (org-document-title        ( :foreground onedark-variant-whitestd
                               :weight 'bold))
  (org-document-info         ( :foreground onedark-variant-whitestd))

  ;; Outshine
  (outshine-level-4 ( :foreground onedark-variant-green))

  ;; Sp
  (sp-pair-overlay-face ( :background onedark-variant-regionbg))

  ;; Company
  (company-tooltip-selection  ( :background onedark-variant-regionbg
                                :distant-foreground onedark-variant-regionds))
  (company-tooltip-mouse      ( :background onedark-variant-regionbg
                                :distant-foreground onedark-variant-regionds))
  (company-tooltip-annotation ( :foreground onedark-variant-gold))
  (company-box-scrollbar      ( :background onedark-variant-regionbg))
  (company-tooltip            ( :foreground onedark-variant-whitestd 
                                :background onedark-variant-doc-box))

  ;; Mode line
  (mode-line                       ( :foreground onedark-variant-whitestd
                                     :background onedark-variant-modelnbg
                                     :box onedark-variant-modebox))
  (mode-line-inactive              ( :foreground onedark-variant-modelninfg
                                     :background onedark-variant-modelninbg))
  (doom-modeline-buffer-modified   ( :foreground onedark-variant-modelndiff))
  (doom-modeline-urgent            ( :foreground onedark-variant-error))
  (doom-modeline-lsp-success       ( :foreground onedark-variant-docs))
  (doom-modeline-buffer-major-mode ( :foreground onedark-variant-blue))
  (doom-modeline-evil-normal-state ( :foreground onedark-variant-eviln
                                     :weight 'bold))

  ;; mu4e
  (mu4e-header-key-face ( :foreground onedark-variant-mu4eheader))

  ;; eshell
  (eshell-prompt ( :foreground onedark-variant-gold))

  ;; Ledger
  (ledger-font-payee-cleared-face   ( :foreground onedark-variant-green))
  (ledger-font-posting-account-face ( :inherit 'font-lock-builtin-face))

  ;; lsp
  (lsp-lsp-flycheck-warning-unnecessary-face ( :inherit 'flycheck-warning))

  ;; lsp-ui
  (lsp-ui-doc-background ( :background onedark-variant-doc-box))

  ;; Flycheck
  (flycheck-warning ( :underline ( :color onedark-variant-warn
                                   :style 'wave)))

  ;; Tab bar
  (tab-bar              ( :foreground onedark-variant-whitestd
                          :background onedark-variant-modelnbg))
  (tab-bar-tab          ( :foreground onedark-variant-white
                          :box ( :line-width -1
                                 :color onedark-variant-modelnbg
                                 :style 'pressed-button)))
  (tab-bar-tab-inactive ( :foreground onedark-variant-whitestd
                          :background onedark-variant-modelnbg))

  ;; Markdown
  (markdown-code-face ( :background onedark-variant-doc-box))

  ;; nusmv
  (nusmv-font-lock-variable-name-face ( :inherit 'font-lock-variable-name-face))
  (nusmv-font-lock-keyword-face       ( :inherit 'font-lock-keyword-face))
  (nusmv-font-lock-constant-face      ( :inherit 'font-lock-constant-face))
  (nusmv-font-lock-type-face          ( :inherit 'font-lock-type-face))
  (nusmv-font-lock-module-name-face   ( :inherit 'font-lock-function-name-face))
  (nusmv-font-lock-directive-face     ( :inherit 'font-lock-preprocessor-face))
  (nusmv-font-lock-separator-face     ( :inherit 'font-lock-builtin-face))

  ;; vterm
  (which-func ( :foreground onedark-variant-blue))

  ;; sudoku
  (sudoku-value-face ( :foreground onedark-variant-mu4eheader
                       :height 2.0))

  ;; Makefile
  (makefile-space ( :background onedark-variant-black))

  ;; slack
  (slack-message-output-text    ( :inherit 'variable-pitch))
  (slack-mrkdwn-code-face       ( :inherit 'org-code))
  (slack-mrkdwn-code-block-face ( :inherit 'org-verbatim))

  ;; General font locks
  (font-lock-function-name-face ( :foreground onedark-variant-blue))
  (font-lock-preprocessor-face  ( :foreground onedark-variant-preproc))
  (font-lock-keyword-face       ( :foreground onedark-variant-keywords))
  (font-lock-string-face        ( :foreground onedark-variant-strings))
  (font-lock-doc-face           ( :foreground onedark-variant-docs))
  (font-lock-comment-face       ( :foreground onedark-variant-comments))
  (font-lock-type-face          ( :foreground onedark-variant-types))
  (font-lock-variable-name-face ( :foreground onedark-variant-vars))
  (font-lock-constant-face      ( :foreground onedark-variant-constants))
  (font-lock-doc-markup-face    ( :foreground onedark-variant-tags))
  (font-lock-builtin-face       ( :foreground onedark-variant-builtin))
 ))

(provide-theme 'onedark-variant)
