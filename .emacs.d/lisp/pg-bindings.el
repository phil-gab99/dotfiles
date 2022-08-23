(use-package hydra
  :straight t
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

(global-set-key (kbd "M-<tab>") 'other-window) ; Bind alt tab to buffer switching

(require 'iso-transl)
(define-key global-map (kbd "<Multi_key>") iso-transl-ctl-x-8-map) ; Bind compose key in case emacs captures it

(use-package general
  :straight t
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

(provide 'pg-bindings)
