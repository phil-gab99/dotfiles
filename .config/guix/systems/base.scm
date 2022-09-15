(define-module (systems base)
  #:use-module (gnu)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services pm)
  #:use-module (gnu services virtualization)
  #:use-module (gnu system nss)
  #:use-module (nongnu packages linux)
  #:use-module (srfi srfi-1))

(use-service-modules nix
                     desktop
                     xorg
                     networking
                     cups
                     ssh)

(use-package-modules certs
                     shells)

(define %charge-thresholds-udev-rule
  (udev-rule
   "90-charge-thresholds.rules"
   (string-append "KERNEL==\"BAT0\", "
                  "SUBSYSTEM==\"power_supply\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp charge /sys/class/power_supply/%k/charge_control_start_threshold /sys/class/power_supply/%k/charge_control_end_threshold\""
                  "\n"
                  "KERNEL==\"BAT0\", "
                  "SUBSYSTEM==\"power_supply\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/power_supply/%k/charge_control_start_threshold /sys/class/power_supply/%k/charge_control_end_threshold\"")))

(define %my-desktop-services
  (modify-services %desktop-services
                   (elogind-service-type config =>
                                         (elogind-configuration
                                          (inherit config)
                                          (handle-lid-switch-external-power 'suspend)))
                   (udev-service-type config =>
                                      (udev-configuration
                                       (inherit config)
                                       (rules (cons %charge-thresholds-udev-rule
                                                    (udev-configuration-rules config)))))
                   (network-manager-service-type config =>
                                                 (network-manager-configuration (inherit config)
                                                                                (vpn-plugins (list network-manager-openvpn))))))

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"

  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"NaturalScrolling\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
  Option \"MiddleEmulation\" \"on\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define-public base-operating-system
  (operating-system
   ;; Use non-free Linux and firmware
   (kernel linux)
   (firmware (list linux-firmware))

   ;; Generic information that may be overriden
   (locale "en_CA.utf8")
   (timezone "America/Toronto")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "tmp")

   ;; Default user
   (users
    (cons
     (user-account (name "phil-gab99")
                   (comment "Philippe Gabriel")
                   (group "users")
                   (home-directory "/home/phil-gab99")
                   (supplementary-groups '("wheel"     ;; sudo
                                           "netdev"    ;; network devices
                                           "kvm"
                                           "tty"
                                           "input"
                                           "libvirt"
                                           "charge"
                                           "docker"
                                           ;; "realtime"  ;; Enable realtime scheduling
                                           "lp"        ;; control bluetooth devices
                                           "audio"     ;; control audio devices
                                           "video")))  ;; control video devices
     %base-user-accounts))

   ;; Add the `charge' group
   (groups
    (cons
     (user-group (system? #t)
                 (name "charge"))
     %base-groups))

   ;; Partition mounted on /boot/efi.
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

   ;; File system to be overridden
   (file-systems
    (cons*
     (file-system (mount-point "/tmp")
                  (device "none")
                  (type "tmpfs")
                  (check? #f))
     %base-file-systems))

   ;; System packages
   (packages
    (append
     (list emacs
           emacs-exwm
           emacs-desktop-environment
           nss-certs     ;; for HTTPS access
           git
           ntfs-3g
           exfat-utils
           fuse-exfat
           vim
           nix
           pulseaudio
           xf86-input-libinput
           gvfs          ;; for user mounts
           xterm
           bluez
           bluez-alsa)
     %base-packages))

   ;; System services
   (services
    (cons*
     (service slim-service-type
              (slim-configuration
               (theme-name "0.x")
               (xorg-configuration
                (xorg-configuration
                 (keyboard-layout keyboard-layout)
                 (extra-config (list %xorg-libinput-config))))))
     (service openssh-service-type)
     (service tor-service-type)
     (service cups-service-type
              (cups-configuration
               (web-interface? #t)
               (extensions
                (list cups-filters))))
     (service nix-service-type)
     (service docker-service-type)
     (service libvirt-service-type
              (libvirt-configuration
               (unix-sock-group "libvirt")
               (tls-port "16555")))
     (service virtlog-service-type
              (virtlog-configuration
               (max-clients 1000)))
     ;; (service thermald-service-type)
     (extra-special-file "/bin/env"
                         (file-append coreutils "/bin/env"))
     (bluetooth-service #:auto-enable? #t)
     (remove (lambda (service)
               (eq? (service-kind service) gdm-service-type))
             %my-desktop-services)))))
