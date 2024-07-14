(define-module (pg systems base)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (srfi srfi-1)
  #:export (system-config))

(use-package-modules admin audio bash compression cups curl emacs file-systems
                     freedesktop fonts gnome gnupg libusb linux
                     package-management python ssh version-control vim wm)
(use-service-modules avahi base cups dbus desktop docker guix linux networking
                     nix pm ssh virtualization xorg)
(use-system-modules accounts pam nss)

(define %acm-udev-rule
  (udev-rule "90-extraacl.rules"
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
  (udev-rule "98-openocd.rules"
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

(define* (system-config #:key system home)
  (operating-system
   (inherit system)

   ;; Use non-free Linux and firmware
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (cons iwlwifi-firmware
                   %base-firmware))

   ;; Virtual camera
   (kernel-loadable-modules (list v4l2loopback-linux-module))

   (locale "en_CA.utf8")
   (timezone "America/Toronto")
   (host-name (operating-system-host-name system))

   ;; Default user
   (users (or (operating-system-users system)
              (cons
               (user-account (name "phil-gab99")
                             (comment "Philippe Gabriel")
                             (group "users")
                             (home-directory "/home/phil-gab99")
                             (supplementary-groups '("wheel"    ;; sudo
                                                     "netdev"   ;; network devices
                                                     "kvm"
                                                     "tty"
                                                     "dialout"
                                                     "uucp"
                                                     "plugdev"
                                                     "input"
                                                     "libvirt"
                                                     "docker"
                                                     "realtime"
                                                     "lp"       ;; control bluetooth devices
                                                     "audio"    ;; control audio devices
                                                     "video"))) ;; control video devices
               %base-user-accounts)))

   (groups
    (append (operating-system-groups system)
            (cons* (user-group
                    (system? #t)
                    (name "uucp"))
                   (user-group
                    (system? #t)
                    (name "plugdev"))
                   (user-group
                    (system? #t)
                    (name "realtime"))
                   %base-groups)))

   (sudoers-file
    (plain-file "sudoers"
                (string-append "\nDefaults verifypw = any\n"
                               (plain-file-content %sudoers-specification)
                               (user-account-name
                                (car (operating-system-users system)))
                               " ALL=(ALL) NOPASSWD:/run/current-system/profile/sbin/halt,/run/current-system/profile/sbin/reboot")))

   ;; System packages
   (packages (cons* bluez
                    bluez-alsa
                    brightnessctl
                    curl
                    emacs-no-x-toolkit
                    exfat-utils
                    flatpak
                    fuse-exfat
                    git
                    gvfs
                    htop
                    lm-sensors
                    net-tools
                    nix
                    openssh
                    python
                    udiskie
                    unzip
                    vim
                    zip
                    %base-packages))

   ;; System services
   (services (append
              (modify-services %base-services
                               (delete login-service-type)
                               (delete mingetty-service-type)
                               (delete console-font-service-type))
              (operating-system-user-services system)
              (list
               (service guix-home-service-type
                        (list
                         (list (user-account-name
                                (car (operating-system-users system)))
                               home)))

               (service elogind-service-type)

               (service console-font-service-type
                        (map (lambda (tty)
                               (cons tty
                                     (file-append
                                      font-terminus
                                      "/share/consolefonts/ter-132n")))
                             '("tty1" "tty2" "tty3")))

               (service greetd-service-type
                        (greetd-configuration
                         (greeter-supplementary-groups (list "video" "input"))
                         (terminals
                          (list (greetd-terminal-configuration
                                 (terminal-vt "1")
                                 (terminal-switch #t)
                                 (source-profile? #f)
                                 (default-session-command
                                   (greetd-wlgreet-sway-session
                                    (wlgreet-session
                                     (greetd-wlgreet-session
                                      (command (file-append bash "/bin/bash"))
                                      (command-args '("-l"))
                                      (background '(0.25 0.25 0.25 0.9))
                                      (headline '(0 0.5 0.5 0.8))
                                      (prompt '(0.22 1 0.08 1))
                                      (prompt-error '(1 0 0 1))
                                      (border '(1 0.84 0 0.5)))))))
                                (greetd-terminal-configuration
                                 (terminal-vt "2")
                                 (source-profile? #f))
                                (greetd-terminal-configuration
                                 (terminal-vt "3")
                                 (source-profile? #f))))))

               (service screen-locker-service-type
                        (screen-locker-configuration
                         (name "swaylock")
                         (program (file-append swaylock "/bin/swaylock"))
                         (using-pam? #t)
                         (using-setuid? #f)))

               (simple-service 'add-nonguix-substitutes
                               guix-service-type
                               (guix-extension
                                (substitute-urls
                                 (cons "https://substitutes.nonguix.org"
                                       %default-substitute-urls))
                                (authorized-keys
                                 (cons (plain-file
                                        "nonguix.pub"
                                        "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
                                       %default-authorized-guix-keys))))

               polkit-wheel-service

               (service network-manager-service-type
                        (network-manager-configuration
                         (vpn-plugins (list network-manager-openvpn
                                            network-manager-openconnect))))
               (service wpa-supplicant-service-type)
               (service modem-manager-service-type)
               (service bluetooth-service-type
                        (bluetooth-configuration
                         (auto-enable? #t)))
               (service usb-modeswitch-service-type)

               (service avahi-service-type)
               (service udisks-service-type)
               (service upower-service-type)
               (service geoclue-service-type)
               (service polkit-service-type)
               (service dbus-root-service-type)
               fontconfig-file-system-service

               (service thermald-service-type)
               (service tlp-service-type
                        (tlp-configuration
                         (cpu-boost-on-ac? #t)
                         (wifi-pwr-on-bat? #t)))

               (service pam-limits-service-type
                        (list
                         (pam-limits-entry "@realtime" 'both 'rtprio 99)
                         (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)
                         (pam-limits-entry "@realtime" 'both 'nice -19)))

               (service kernel-module-loader-service-type '("v4l2loopback"))
               (simple-service 'v4l2loopback-config etc-service-type
                               (list `("modprobe.d/v4l2loopback.conf"
                                       ,(plain-file "v4l2loopback.conf"
                                                    "options v4l2loopback devices=1 video_nr=2 exclusive_caps=1 card_label=\"OBS Virtual Camera\""))))

               (service docker-service-type)
               (service libvirt-service-type
                        (libvirt-configuration
                         (unix-sock-group "libvirt")
                         (tls-port "16555")))
               (service virtlog-service-type
                        (virtlog-configuration
                         (max-clients 1000)))

               (service openssh-service-type
                        (openssh-configuration
                         (openssh openssh-sans-x)))

               (service sane-service-type)
               (service cups-pk-helper-service-type)
               (service cups-service-type
                        (cups-configuration
                         (web-interface? #t)
                         (extensions (list cups-filters))))

               (service x11-socket-directory-service-type)
               
               (service ntp-service-type)

               (service nix-service-type)

               (simple-service 'mtp udev-service-type (list libmtp)) 

               (extra-special-file "/bin/env"
                                   (file-append coreutils "/bin/env"))
               (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                                   (file-append glibc "/lib/ld-linux-x86-64.so.2")))))
   (name-service-switch %mdns-host-lookup-nss)))
