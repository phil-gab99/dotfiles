(define-module (systems base)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:use-module (srfi srfi-1))

(use-package-modules audio
                     base
                     certs
                     cups
                     dns
                     emacs
                     emacs-xyz
                     file-systems
                     gnome
                     gtk
                     linux
                     mtools
                     ncurses
                     package-management
                     pulseaudio
                     shells
                     version-control
                     vim
                     virtualization
                     web-browsers
                     wm
                     xorg)

(use-service-modules base
                     cups
                     desktop
                     docker
                     networking
                     nix
                     pm
                     ssh
                     virtualization
                     xorg)

(use-system-modules nss)

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

(define %acm-udev-rule
  (udev-rule
   "90-extraacl.rules"
   (string-append "KERNEL==\"ttyUSB[0-9]*\", "
                  "TAG+=\"udev-acl\", "
                  "TAG+=\"uaccess\", "
                  "OWNER=\"phil-gab99\""
                  "\n"
                  "KERNEL==\"ttyACM[0-9]*\", "
                  "TAG+=\"udev-acl\", "
                  "TAG+=\"uaccess\", "
                  "OWNER=\"phil-gab99\"")))

(define %open-ocd-udev-rule
  (udev-rule
   "98-openocd.rules"
   (string-append "ACTION!=\"add|change\", "
                  "GOTO=\"openocd_rules_end\""
                  "\n"
                  "SUBSYSTEM!=\"usb|tty|hidraw\", "
                  "GOTO=\"openocd_rules_end\""
                  "\n"
                  "#Please keep this list sorted by VID:PID"
                  "\n"
                  "#CMSIS-DAP compatible adapters"
                  "ATTRS{product}==\"*CMSIS-DAP*\", "
                  "MODE=\"664\", "
                  "GROUP=\"plugdev\""
                  "\n"
                  "LABEL=\"openocd_rules_end\"")))

;; Define rest of rules

(define %my-desktop-services
  (modify-services %desktop-services
    (elogind-service-type config =>
                          (elogind-configuration
                           (inherit config)
                           (handle-lid-switch-external-power 'suspend)))
    (udev-service-type config =>
                       (udev-configuration
                        (inherit config)
                        (rules (cons* %charge-thresholds-udev-rule
                                      (udev-configuration-rules config)))))
    (network-manager-service-type config =>
                                  (network-manager-configuration
                                   (inherit config)
                                   (vpn-plugins (list network-manager-openvpn
                                                      network-manager-openconnect))))
    (guix-service-type config =>
                       (guix-configuration
                        (inherit config)
                        (substitute-urls
                         (append (list "https://substitutes.nonguix.org")
                                 %default-substitute-urls))
                        (authorized-keys
                         (append (list (local-file (string-append (getenv "XDG_CONFIG_HOME")
                                                                  "/guix/systems/signing-key.pub")))
                                 %default-authorized-guix-keys))))))

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
                                           "dialout"
                                           "uucp"
                                           "plugdev"
                                           "input"
                                           "libvirt"
                                           "charge"
                                           "docker"
                                           "lp"        ;; control bluetooth devices
                                           "audio"     ;; control audio devices
                                           "video")))  ;; control video devices
     %base-user-accounts))

   ;; Add extra groups
   (groups
    (cons*
     (user-group (system? #t)
                 (name "charge"))
     (user-group (system? #t)
                 (name "uucp"))
     (user-group (system? #t)
                 (name "plugdev"))
     %base-groups))

   ;; Partition mounted on /boot/efi.
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

   ;; File system to be overridden
   (file-systems
    (cons
     (file-system (mount-point "/tmp")
                  (device "none")
                  (type "tmpfs")
                  (check? #f))
     %base-file-systems))

   ;; System packages
   (packages
    (append
     (list bluez
           bluez-alsa
           coreutils
           emacs
           emacs-exwm
           emacs-desktop-environment
           exfat-utils
           fuse-exfat
           git
           gvfs
           ncurses
           nix
           nss-certs
           ntfs-3g
           pulseaudio
           vim
           xf86-input-libinput
           xterm)
     %base-packages))

   ;; System services
   (services
    (cons*
     (set-xorg-configuration
      (xorg-configuration
       (keyboard-layout keyboard-layout)
       (extra-config (list %xorg-libinput-config))))
     (service openssh-service-type)
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
     (service bluetooth-service-type
              (bluetooth-configuration
               (auto-enable? #t)))
     (service xfce-desktop-service-type)
     (extra-special-file "/bin/env"
                         (file-append coreutils "/bin/env"))
     (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                         (file-append glibc "/lib/ld-linux-x86-64.so.2"))
     %my-desktop-services))))
