;;; pg-onelight-variant-theme.el --- One Light Variant color theme
;; Author: Philippe Gabriel

(deftheme pg-onelight-variant
  "One Light Variant - My pg-onelight-variant custom theme.")

(defvar pg-onelight-variant-colors-alist
  '(("pg-onelight-variant-blackstd"    . "#333333")
    ("pg-onelight-variant-black"       . "#282c34")
    ("pg-onelight-variant-gold"        . "#eead0e")
    ("pg-onelight-variant-whitebg"     . "#ffffff")
    ("pg-onelight-variant-dark"        . "#000000")
    ("pg-onelight-variant-doc-box"     . "#cacaca")
    ("pg-onelight-variant-cursorbg"    . "#4682d9")
    ("pg-onelight-variant-regionbg"    . "#b4eeb4")
    ("pg-onelight-variant-regionds"    . "#828997")
    ("pg-onelight-variant-blue"        . "#00bfff")
    ("pg-onelight-variant-green"       . "#2cff64")
    ("pg-onelight-variant-keywords"    . "#c678dd")
    ("pg-onelight-variant-strings"     . "#50A14F")
    ("pg-onelight-variant-docs"        . "#008b00")
    ("pg-onelight-variant-modebox"     . "#7f7f7f")
    ("pg-onelight-variant-comments"    . "#424243")
    ("pg-onelight-variant-codeblockbg" . "#eee9e9")
    ("pg-onelight-variant-code"        . "#ffa500")
    ("pg-onelight-variant-verbatim"    . "#00dd00")
    ("pg-onelight-variant-table"       . "#68228b")
    ("pg-onelight-variant-types"       . "#cd853f")
    ("pg-onelight-variant-vars"        . "#ff6347")
    ("pg-onelight-variant-link"        . "#61afef")
    ("pg-onelight-variant-constants"   . "#eead0e")
    ("pg-onelight-variant-tags"        . "#ee00ee")
    ("pg-onelight-variant-builtin"     . "#008b8b")
    ("pg-onelight-variant-preproc"     . "#7b447b")
    ("pg-onelight-variant-shexec"      . "#afeeee")
    ("pg-onelight-variant-modelnfg"    . "#030303")
    ("pg-onelight-variant-modelnbg"    . "#bfbfbf")
    ("pg-onelight-variant-modelndiff"  . "#ff4c35")
    ("pg-onelight-variant-modelninfg"  . "#1c1c1c")
    ("pg-onelight-variant-modelninbg"  . "#a6a6a6")
    ("pg-onelight-variant-lnnumfg"     . "#262626")
    ("pg-onelight-variant-lnnumbg"     . "#999999")
    ("pg-onelight-variant-lnsltfg"     . "#fefefe")
    ("pg-onelight-variant-mu4eheader"  . "#ff83fa")
    ("pg-onelight-variant-parenmatch"  . "#c678dd")
    ("pg-onelight-variant-error"       . "#ff6666")
    ("pg-onelight-variant-warn"        . "#8b5a00")
    ("pg-onelight-variant-minibuf"     . "#0000ff")
    ("pg-onelight-variant-eviln"       . "#818ff4"))
  "List of One Dark Variant colors.")

(defmacro pg-onelight-variant-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    pg-onelight-variant-colors-alist))
     ,@body))

(pg-onelight-variant-with-color-variables
  (custom-theme-set-faces
   'pg-onelight-variant

   ;; General
   `(cursor                   ((t ( :background ,pg-onelight-variant-cursorbg))))
   `(default                  ((t ( :foreground ,pg-onelight-variant-blackstd
                                    :background ,pg-onelight-variant-whitebg))))
   `(italic                   ((t ( :slant italic))))
   `(link                     ((t ( :foreground ,pg-onelight-variant-link
                                    :weight bold
                                    :underline t))))
   `(region                   ((t ( :background ,pg-onelight-variant-regionbg
                                    :distant-foreground ,pg-onelight-variant-regionds))))
   `(highlight                ((t ( :background ,pg-onelight-variant-regionbg
                                    :distant-foreground ,pg-onelight-variant-regionds))))
   `(show-paren-match         ((t ( :foreground ,pg-onelight-variant-parenmatch
                                    :weight bold))))
   `(sh-quoted-exec           ((t ( :foreground ,pg-onelight-variant-shexec))))
   `(line-number              ((t ( :foreground ,pg-onelight-variant-lnnumfg
                                    :background ,pg-onelight-variant-lnnumbg))))
   `(line-number-current-line ((t ( :foreground ,pg-onelight-variant-lnsltfg
                                    :background ,pg-onelight-variant-lnnumbg))))
   `(minibuffer-prompt        ((t ( :foreground ,pg-onelight-variant-minibuf))))
   `(shadow                   ((t ( :foreground ,pg-onelight-variant-dark))))
   `(error                    ((t ( :foreground ,pg-onelight-variant-error))))
   `(warning                  ((t ( :foreground ,pg-onelight-variant-warn))))
   `(message-header-name      ((t ( :bold t))))

   ;; Persp
   `(persp-selected-face ((t ( :foreground ,pg-onelight-variant-warn))))

   ;; Man
   `(Man-overstrike ((t ( :foreground ,pg-onelight-variant-types
                          :weight bold))))
   `(Man-underline  ((t ( :foreground ,pg-onelight-variant-keywords
                          :underline t))))

   ;; Info
   `(Info-quoted    ((t ( :foreground ,pg-onelight-variant-code
                          :inherit (fixed-pitch)))))

   ;; Org
   `(org-hide                  ((t ( :foreground ,pg-onelight-variant-whitebg))))
   `(org-level-4               ((t ( :foreground ,pg-onelight-variant-green))))
   `(org-date                  ((t ( :foreground ,pg-onelight-variant-blue))))
   `(org-document-info-keyword ((t ( :inherit (font-lock-comment-face
                                               fixed-pitch)))))
   `(org-document-title        ((t ( :foreground ,pg-onelight-variant-blackstd
                                     :weight bold))))
   `(org-document-info         ((t ( :foreground ,pg-onelight-variant-blackstd))))
   `(org-ellipsis              ((t ( :underline nil))))
   `(org-block                 ((t ( :foreground nil
                                     :background ,pg-onelight-variant-codeblockbg
                                     :inherit (fixed-pitch)))))
   `(org-code                  ((t ( :foreground ,pg-onelight-variant-code
                                     :inherit (fixed-pitch)))))
   `(org-verbatim              ((t ( :foreground ,pg-onelight-variant-verbatim
                                     :inherit (fixed-pitch)))))
   `(org-table                 ((t ( :foreground ,pg-onelight-variant-table
                                     :inherit (shaodw
                                               fixed-pitch)))))
   `(org-indent                ((t ( :inherit (org-hide
                                               fixed-pitch)))))
   `(org-special-keyword       ((t ( :inherit (font-lock-comment-face
                                               fixed-pitch)))))
   `(org-meta-line             ((t ( :inherit (font-lock-comment-face
                                               fixed-pitch)))))
   `(org-checkbox              ((t ( :inherit (fixed-pitch)))))

   ;; Outshine
   `(outshine-level-4 ((t ( :foreground ,pg-onelight-variant-green))))

   ;; Sp
   `(sp-pair-overlay-face ((t ( :background ,pg-onelight-variant-regionbg))))

   ;; Company
   `(company-tooltip-selection  ((t ( :background ,pg-onelight-variant-regionbg
                                      :distant-foreground ,pg-onelight-variant-regionds))))
   `(company-tooltip-mouse      ((t ( :background ,pg-onelight-variant-regionbg
                                      :distant-foreground ,pg-onelight-variant-regionds))))
   `(company-tooltip-annotation ((t ( :foreground ,pg-onelight-variant-gold))))
   `(company-box-scrollbar      ((t ( :background ,pg-onelight-variant-regionbg))))
   `(company-tooltip            ((t ( :foreground ,pg-onelight-variant-blackstd 
                                      :background ,pg-onelight-variant-doc-box))))

   ;; Mode line
   `(mode-line                       ((t ( :foreground ,pg-onelight-variant-modelnfg
                                           :background ,pg-onelight-variant-modelnbg
                                           :box ,pg-onelight-variant-modebox))))
   `(mode-line-inactive              ((t ( :foreground ,pg-onelight-variant-modelninfg
                                           :background ,pg-onelight-variant-modelninbg))))
   `(doom-modeline-buffer-modified   ((t ( :foreground ,pg-onelight-variant-modelndiff))))
   `(doom-modeline-urgent            ((t ( :foreground ,pg-onelight-variant-error))))
   `(doom-modeline-lsp-success       ((t ( :foreground ,pg-onelight-variant-docs))))
   `(doom-modeline-buffer-major-mode ((t ( :foreground ,pg-onelight-variant-tags))))
   `(doom-modeline-evil-normal-state ((t ( :foreground ,pg-onelight-variant-eviln
                                           :weight bold))))

   ;; mu4e
   `(mu4e-header-key-face ((t ( :foreground ,pg-onelight-variant-mu4eheader))))

   ;; eshell
   `(eshell-prompt ((t ( :foreground ,pg-onelight-variant-gold))))

   ;; Ledger
   `(ledger-font-payee-cleared-face   ((t ( :foreground ,pg-onelight-variant-green))))
   `(ledger-font-posting-account-face ((t ( :inherit (font-lock-builtin-face)))))

   ;; lsp
   `(lsp-lsp-flycheck-warning-unnecessary-face ((t ( :inherit (flycheck-warning)))))

   ;; lsp-ui
   `(lsp-ui-doc-background ((t ( :background ,pg-onelight-variant-doc-box))))

   ;; Flycheck
   `(flycheck-warning ((t ( :underline ( :color ,pg-onelight-variant-warn
                                         :style wave)))))

   ;; Tab bar
   `(tab-bar              ((t ( :foreground ,pg-onelight-variant-blackstd
                                :background ,pg-onelight-variant-modelnbg))))
   `(tab-bar-tab          ((t ( :foreground ,pg-onelight-variant-dark
                                :box ( :line-width -1
                                       :color ,pg-onelight-variant-modelnbg
                                       :style pressed-button)))))
   `(tab-bar-tab-inactive ((t ( :foreground ,pg-onelight-variant-blackstd
                                :background ,pg-onelight-variant-modelnbg))))

   ;; Markdown
   `(markdown-code-face ((t ( :background ,pg-onelight-variant-doc-box))))

   ;; nusmv
   `(nusmv-font-lock-variable-name-face ((t ( :inherit (font-lock-variable-name-face)))))
   `(nusmv-font-lock-keyword-face       ((t ( :inherit (font-lock-keyword-face)))))
   `(nusmv-font-lock-constant-face      ((t ( :inherit (font-lock-constant-face)))))
   `(nusmv-font-lock-type-face          ((t ( :inherit (font-lock-type-face)))))
   `(nusmv-font-lock-module-name-face   ((t ( :inherit (font-lock-function-name-face)))))
   `(nusmv-font-lock-directive-face     ((t ( :inherit (font-lock-preprocessor-face)))))
   `(nusmv-font-lock-separator-face     ((t ( :inherit (font-lock-builtin-face)))))

   ;; vterm
   `(which-func ((t ( :foreground ,pg-onelight-variant-blue))))

   ;; sudoku
   `(sudoku-value-face ((t ( :foreground ,pg-onelight-variant-mu4eheader
                             :height 2.0))))

   ;; Makefile
   `(makefile-space ((t ( :background ,pg-onelight-variant-whitebg))))

   ;; slack
   `(slack-message-output-text    ((t ( :inherit (variable-pitch)))))
   `(slack-mrkdwn-code-face       ((t ( :inherit (org-code)))))
   `(slack-mrkdwn-code-block-face ((t ( :inherit (org-verbatim)))))

   ;; General font locks
   `(font-lock-function-name-face ((t ( :foreground ,pg-onelight-variant-blue))))
   `(font-lock-preprocessor-face  ((t ( :foreground ,pg-onelight-variant-preproc))))
   `(font-lock-keyword-face       ((t ( :foreground ,pg-onelight-variant-keywords))))
   `(font-lock-string-face        ((t ( :foreground ,pg-onelight-variant-strings))))
   `(font-lock-doc-face           ((t ( :foreground ,pg-onelight-variant-docs
                                        :slant italic))))
   `(font-lock-comment-face       ((t ( :foreground ,pg-onelight-variant-comments
                                        :slant italic))))
   `(font-lock-type-face          ((t ( :foreground ,pg-onelight-variant-types))))
   `(font-lock-variable-name-face ((t ( :foreground ,pg-onelight-variant-vars))))
   `(font-lock-constant-face      ((t ( :foreground ,pg-onelight-variant-constants))))
   `(font-lock-doc-markup-face    ((t ( :foreground ,pg-onelight-variant-tags))))
   `(font-lock-builtin-face       ((t ( :foreground ,pg-onelight-variant-builtin))))))

(provide-theme 'pg-onelight-variant)
