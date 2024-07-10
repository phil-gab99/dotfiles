(define-module (pg systems vm-host)
  #:use-module (pg systems base)
  #:use-module (pg home services bash)
  #:use-module (pg home services udiskie)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services dotfiles))

;; Use dummy dotfiles for minimal sway config

(use-package-modules file-systems
                     fonts
                     gnome
                     gnome-xyz
                     music
                     terminals
                     video
                     wm
                     xdisorg)
(use-service-modules spice)
(use-system-modules vm)

(define test-os
  (system-config
   #:home
   (home-environment
    (packages (list adwaita-icon-theme
                    foot
                    fuzzel
                    font-iosevka-aile
                    font-jetbrains-mono
                    gammastep
                    grimshot
                    mako
                    papirus-icon-theme
                    playerctl
                    sway
                    swaylock
                    swayidle
                    swaybg))
    (services (list (service home-bash-service-type)
                    (service home-udiskie-service-type)
                    (service home-dotfiles-service-type
                             (home-dotfiles-configuration
                              (source-directory "/home/phil-gab99/.dotfiles")
                              (directories '(".files")))))))
   #:system
   (operating-system
     (host-name "vm-host")
     (keyboard-layout (keyboard-layout "us"))

     (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets (list "/boot/efi"))
                  (keyboard-layout keyboard-layout)))

     (users
      (cons (user-account (name "phigabri")
                          (comment "Philippe Gabriel")
                          (password (crypt "123" "$6$abc"))
                          (group "users")
                          (home-directory "/home/phigabri")
                          (supplementary-groups '("wheel"
                                                  "netdev"
                                                  "kvm"
                                                  "tty"
                                                  "input"
                                                  "audio"
                                                  "video")))
            %base-user-accounts))

     (file-systems
      (cons (file-system (device "none")
                         (mount-point "/tmp")
                         (type "tmpfs"))
            %base-file-systems))

     (services (list (service spice-vdagent-service-type))))))

(virtualized-operating-system test-os)
