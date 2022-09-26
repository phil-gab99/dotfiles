;;; pg-bindings.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'general)
(with-eval-after-load 'which-key
  (with-eval-after-load 'evil
    (require 'general)))

(with-eval-after-load 'general
  (general-create-definer pg/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(straight-use-package 'hydra)
(unless (fboundp 'defhydra)
  (autoload #'defhydra "hydra"))

(defhydra hydra-text-scale (:timeout 5)
  "Scale text."
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out"))

(defhydra hydra-window-scale (:timeout 5)
  "Scale window."
  ("<" evil-window-decrease-width "width dec")
  (">" evil-window-increase-width "width inc")
  ("-" evil-window-decrease-height "height dec")
  ("+" evil-window-increase-height "height inc")
  ("=" balance-windows "balance"))

(defhydra hydra-x-window-scale (:timeout 5)
  "Scale x window."
  ("<" (exwm-layout-shrink-window-horizontally 50) "width dec")
  (">" (exwm-layout-enlarge-window-horizontally 50) "width inc")
  ("-" (exwm-layout-shrink-window 50) "height dec")
  ("+" (exwm-layout-enlarge-window 50) "height inc")
  ("w" exwm-floating-toggle-floating "float toggle")
  ("f" exwm-layout-set-fullscreen "fullscreen"))

(defhydra hydra-window-move (:timeout 5)
  "Move window."
  ("h" windmove-left "left")
  ("l" windmove-right "right")
  ("j" windmove-down "down")
  ("k" windmove-up "up"))

(defhydra hydra-window-swap (:timeout 5)
  "Swap window."
  ("h" windmove-swap-states-left "left")
  ("l" windmove-swap-states-right "right")
  ("j" windmove-swap-states-down "down")
  ("k" windmove-swap-states-up "up"))

(defhydra hydra-window-change (:timeout 5)
  "Change window."
  ("l" next-buffer "right")
  ("h" previous-buffer "left"))

(defhydra hydra-eyebrowse-switch (:timeout 5)
  "Switch workspace."
  ("l" eyebrowse-next-window-config "next")
  ("h" eyebrowse-prev-window-config "prev"))

(defhydra hydra-perspective-switch (:timeout 5)
  "Switch perspective."
  ("l" persp-next "next")
  ("h" persp-prev "prev"))

(with-eval-after-load 'general
  (pg/leader-keys
    "s" '(:ignore t :which-key "scaling")
    "st" '(hydra-text-scale/body :which-key "scale text")
    "sw" '(hydra-window-scale/body :which-key "scale window")
    "sx" '(hydra-x-window-scale/body :which-key "scale x window")

    "w" '(:ignore t :which-key "window")
    "wm" '(hydra-window-move/body :which-key "move")
    "ws" '(hydra-window-swap/body :which-key "swap")
    "wc" '(hydra-window-change/body :which-key "change")))

(require 'iso-transl)
(with-eval-after-load 'iso-transl
  (global-set-key (kbd "<Multi_key>") #'iso-transl-ctl-x-8-map))

(provide 'pg-bindings)
