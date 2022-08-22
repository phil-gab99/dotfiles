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
(setq vc-follow-symlinks t)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(dolist (mode '(org-mode-hook         ; Disable line numbers for some modes
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
  (add-hook mode (lambda() (display-line-numbers-mode 0))))
