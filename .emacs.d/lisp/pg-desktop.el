(straight-use-package '(app-launcher :host github
                                     :repo "SebastienWae/app-launcher"))
(require 'app-launcher)

(defun pg/exwm-update-class ()
  "Sets buffer names to be app names."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pg/configure-window-by-class ()
  "Per application configuration."
  (pcase exwm-class-name
    ("qutebrowser" (exwm-layout-hide-mode-line))
    ("mpv" (exwm-layout-hide-mode-line))
    ("PPSSPPSDL" (exwm-layout-hide-mode-line))
    ("edu-mit-csail-sdg-alloy4whole-Alloy" (exwm-layout-hide-mode-line))))

(defun pg/run-in-background (command)
  "Runs a process in the background."
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun pg/herd-service (service)
  "Starts herd process."
  (start-process-shell-command "herd start" nil (concat "herd start " service)))

(defun pg/set-wallpaper ()
  "Sets desktop wallpaper."
  (interactive)
  (pg/herd-service "feh"))

(defun pg/update-displays ()
  "Multiple display management."
  (interactive)
  (pg/herd-service "autorandr")
  ;; Change this with respect to the different screen configuration
  ;; Check arandr for display names
  (customize-set-variable 'exwm-randr-workspace-monitor-plist
                          (pcase (string-trim (shell-command-to-string "autorandr --detected"))
                            ("work" '(5 "HDMI-1"))
                            ("work+" '(5 "HDMI-1"))
                            ("jclab" '(5 "HDMI-1"))
                            ("jb-0305" '(5 "HDMI-1"))
                            ("jb-4205" '(5 "HDMI-1"))
                            ("aa-1140" '(5 "HDMI-1"))
                            ("aa-1207" '(5 "HDMI-1"))
                            ("aa-1340" '(5 "HDMI-1"))
                            ("aa-2369" '(5 "HDMI-1"))
                            ("rg-e310" '(5 "HDMI-1"))
                            ("jc-s139" '(5 "HDMI-1"))
                            ("jc-s1139" '(5 "DP-1"))
                            ("rg-s142" '(5 "HDMI-1"))
                            ("rg-p310" '(5 "DP-1"))
                            ("cm-z240" '(5 "HDMI-1"))
                            ("entertainment" '(5 "HDMI-1"))))
  (pg/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(defun pg/exwm-startup ()
  "Initializations."
  (pg/start-panel)
  (pg/herd-service "compton")
  (pg/herd-service "xsettingsd")
  (pg/herd-service "dunst")
  (pg/herd-service "nm-applet")
  (pg/herd-service "syncthing-gtk")
  (pg/herd-service "xss-lock")
  (pg/herd-service "udiskie")
  (pg/herd-service "pasystray")
  (pg/herd-service "xmodmap"))

(unless pg/is-guix-system
  (straight-use-package 'exwm))
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-input)
(with-eval-after-load 'exwm

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'pg/exwm-update-class)
  ;; When exwm starts up
  (add-hook 'exwm-init-hook #'pg/exwm-startup)
  ;; Configure launching of some x windows
  (add-hook 'exwm-manage-finish-hook #'pg/configure-window-by-class)

  (pg/customize-set-variables
   `((exwm-workspace-number . 6)
     (exwm-workspace-show-all-buffers . t)
     (exwm-input-prefix-keys . (?\C-x
                                ?\C-g
                                ?\C-h
                                ?\M-x
                                ?\M-`
                                ?\M-&
                                ?\M-:
                                ?\C-\s))
     ;; Reset to line-mode (C-c C-k switches to char-mode via
     ;; `exwm-input-release-keyboard')
     (exwm-input-global-keys . (([?\s-r] . exwm-reset)

                                ([?\s-n] . (lambda ()
                                             (interactive)
                                             (pg/dunstctl "history-pop")))

                                ([?\s-c] . (lambda ()
                                             (interactive)
                                             (pg/dunstctl "close")))

                                ([?\s-\s] . app-launcher-run-app)

                                ;; Switch between char and line mode
                                ([?\s-s] . exwm-input-toggle-keyboard)

                                ;; Launch applications via shell command
                                ([?\s-t] . (lambda (command)
                                             (interactive (list (read-shell-command "$ ")))
                                             (start-process-shell-command command nil command)))

                                ;; Switch workspace
                                ([?\s-w] . exwm-workspace-switch)

                                ;; Bind the tilde key to workspace 0 when
                                ;; switching/creating
                                ([?\s-`] . (lambda ()
                                             (interactive)
                                             (exwm-workspace-switch-create 0)))

                                ;; 's-N': Switch to certain workspace with Super
                                ;; plus a number key (0 - 9)
                                ,@(mapcar (lambda (i)
                                            `(,(kbd (format "s-%d" i)) .
                                              (lambda ()
                                                (interactive)
                                                (exwm-workspace-switch-create ,i))))
                                          (number-sequence 0 9))))
     (exwm-manage-configurations . (((string-equal exwm-class-name "Nyxt") char-mode t)))))
  (global-set-key (kbd "C-x B") #'exwm-workspace-switch-to-buffer)
  (define-key exwm-mode-map (kbd "C-q") #'exwm-input-send-next-key)

  (pg/update-displays)

  (exwm-randr-enable)
  (exwm-enable))

(unless pg/is-guix-system
  (straight-use-package 'desktop-environment))
(with-eval-after-load 'exwm
  (require 'desktop-environment))
(with-eval-after-load 'desktop-environment
  (pg/customize-set-variables
   '((desktop-environment-brightness-normal-increment . "5%+")
     (desktop-environment-brightness-normal-decrement . "5%-")
     (desktop-environment-music-toggle-command . "mpc toggle")))
  (desktop-environment-mode)
  (if (fboundp 'diminish)
      (diminish #'desktop-environment-mode)
    (with-eval-after-load 'diminish
      (diminish #'desktop-environment-mode))))

(defvar pg/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun pg/kill-panel ()
  "Kills active polybar panel."
  (interactive)
  (when pg/polybar-process
    (ignore-errors
      (kill-process pg/polybar-process)))
  (setq pg/polybar-process nil))

(defun pg/start-panel ()
  "Starts a polybar panel."
  (interactive)
  (pg/kill-panel)
  (setq pg/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun pg/send-polybar-hook (module-name hook-index)
  "Displays message using polybar."
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun pg/exwm-workspace-current-index ()
  "Displays current workspace."
  (concat "WS: " (int-to-string exwm-workspace-current-index)))

(defun pg/send-polybar-exwm-workspace ()
  "Sends workspace information to polybar."
  (pg/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'pg/send-polybar-exwm-workspace)

(defun pg/dunstctl (cmd)
  "Calls dunst special commands."
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " cmd)))

(defun pg/disable-desktop-notifications ()
  "Stops notifications from popping."
  (interactive)
  (start-process-shell-command "dunstctl" nil "dunstctl set-paused true"))

(defun pg/enable-desktop-notifications ()
  "Enables notifications to pop."
  (interactive)
  (start-process-shell-command "dunstctl" nil "dunstctl set-paused false"))

(provide 'pg-desktop)
