;; Sets buffer names to be app names
(defun pg/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pg/set-wallpaper ()
  (interactive)
  (start-process-shell-command "feh" nil "feh --bg-scale ~/Pictures/ferdinand-stohr-NFs6dRTBgaM-unsplash.jpg"))
  ;; (start-process-shell-command "feh" nil "feh --bg-scale /usr/share/backgrounds/System76-Fractal_Mountains-by_Kate_Hazen_of_System76.png"))

;; Some configurations to do per application
(defun pg/configure-window-by-class ()
  (pcase exwm-class-name
    ("qutebrowser" (exwm-layout-hide-mode-line))
    ("mpv" (exwm-layout-hide-mode-line))
    ("PPSSPPSDL" (exwm-layout-hide-mode-line))
    ("edu-mit-csail-sdg-alloy4whole-Alloy" (exwm-layout-hide-mode-line))))

;; Runs a process in the background
(defun pg/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; For managing multiple displays
(defun pg/update-displays ()
  (interactive)
  (pg/run-in-background "autorandr --change --force")
  ;; Change this with respect to the different screen configuration
  ;; Check arandr for display names
  (setq exwm-randr-workspace-monitor-plist
        (pcase (shell-command-to-string "autorandr --detected")
          ("work\n" '(5 "HDMI-1"))
          ("work+\n" '(5 "HDMI-1"))
          ("jclab\n" '(5 "HDMI-1"))
          ("aa-1140\n" '(5 "HDMI-1"))
          ("rg-e310\n" '(5 "HDMI-1"))
          ("jc-s139\n" '(5 "HDMI-1"))
          ("jc-s1139\n" '(5 "DP-1"))
          ("entertainment\n" '(5 "HDMI-1"))))
  (pg/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

;; Some initializations
(defun pg/exwm-startup ()
  (pg/start-panel)
  (pg/run-in-background "dunst")
  (pg/run-in-background "nm-applet")
  (pg/run-in-background "udiskie -t")
  (pg/run-in-background "pasystray"))

(require 'exwm-config)
(require 'exwm-randr)
(use-package exwm
  :straight t
  :custom
  (exwm-manage-configurations
   '(((string-equal exwm-class-name "Nyxt")
      char-mode t)))
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 6)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'pg/exwm-update-class)

  ;; When exwm starts up
  (add-hook 'exwm-init-hook #'pg/exwm-startup)

  ;; Configure launching of some x windows
  (add-hook 'exwm-manage-finish-hook #'pg/configure-window-by-class)

  ;; Configure some keybindings
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; Display all buffers across all workspaces when switching
  (setq exwm-workspace-show-all-buffers t)


  ;; Smart display adaptation
  (add-hook 'exwm-randr-screen-change-hook #'pg/update-displays)
  (pg/update-displays)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-g
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\s ;; C-SPC
          M-tab))

  ;; C-q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        `(([?\s-r] . exwm-reset)

          ;; Switch between char and line mode
          ([?\s-s] . exwm-input-toggle-keyboard)

          ;; Launch applications via shell command
          ([?\s-t] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; Bind the tilde key to workspace 0 when switching/creating
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app)
  (exwm-randr-enable)
  (exwm-enable)
  :bind (("C-x B" . exwm-workspace-switch-to-buffer)))

(use-package desktop-environment
  :straight t
  :after exwm
  :diminish desktop-environment-mode
  :config
  (desktop-environment-mode)
  (unbind-key "<XF86AudioPlay>" 'desktop-environment-mode-map)
  :custom
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(defvar pg/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun pg/kill-panel ()
  (interactive)
  (when pg/polybar-process
    (ignore-errors
      (kill-process pg/polybar-process)))
  (setq pg/polybar-process nil))

(defun pg/start-panel ()
  (interactive)
  (pg/kill-panel)
  (setq pg/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun pg/send-polybar-hook (module-name hook-index)
    (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun pg/exwm-workspace-current-index ()
    (concat "WS: " (int-to-string exwm-workspace-current-index)))

(defun pg/send-polybar-exwm-workspace ()
  (pg/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'pg/send-polybar-exwm-workspace)

(defun pg/dunstctl (cmd)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " cmd)))

(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (pg/dunstctl "history-pop")))
(exwm-input-set-key (kbd "s-c") (lambda () (interactive) (pg/dunstctl "close")))

(defun pg/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun pg/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(provide 'pg-desktop)
