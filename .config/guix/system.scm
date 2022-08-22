;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
 (gnu)
 (srfi srfi-1)
 (gnu services)
 (gnu services pm)
 (gnu services cups)
 (gnu services desktop)
 (gnu services docker)
 (gnu services networking)
 (gnu services virtualization)
 (gnu packages audio)
 (gnu packages cups)
 (gnu packages dns)
 (gnu packages emacs)
 (gnu packages file-systems)
 (gnu packages gtk)
 (gnu packages gnome)
 (gnu packages linux)
 (gnu packages mtools)
 (gnu packages package-management)
 (gnu packages pulseaudio)
 (gnu packages version-control)
 (gnu packages vim)
 (gnu packages virtualization)
 (gnu packages web-browsers)
 (gnu packages wm)
 (gnu packages xorg)
 (nongnu packages linux)
 (nongnu system linux-initrd))

(use-service-modules
 cups
 desktop
 networking
 ssh
 nix
 xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_CA.utf8")
 (timezone "America/Toronto")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "s76-laptop")
 (users (cons* (user-account
                (name "phil-gab99")
                (comment "Philippe Gabriel")
                (group "users")
                (home-directory "/home/phil-gab99")
                (supplementary-groups
                 '("wheel"
                   "kvm"
                   "lp"
                   "input"
                   "libvirt"
                   "netdev"
                   "audio"
                   "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "emacs")
         (specification->package "emacs-exwm")
         (specification->package "emacs-desktop-environment")
         (specification->package "nss-certs")
         git
         ntfs-3g
         exfat-utils
         fuse-exfat
         vim
         dnsmasq
         nix
         pulseaudio
         sysfsutils
         tlp
         xf86-input-libinput
         gvfs)
   %base-packages))
 (services
  (cons*
   (service openssh-service-type)
   (service tor-service-type)
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (extensions
              (list cups-filters))))
   (service nix-service-type)
   (service thermald-service-type)
   (service bluetooth-service-type)
   (service libvirt-service-type
            (libvirt-configuration
             (unix-sock-group "libvirt")
             (tls-port "16555")))
   (service virtlog-service-type
            (virtlog-configuration
             (max-clients 1000)))
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout keyboard-layout)))
   (remove (lambda (service)
             (eq? (service-kind service) gdm-service-type))
           %desktop-services)))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets (list "/boot/efi"))
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (swap-space
         (target
          (uuid "5c2ecf42-19e0-46c0-ba33-51ced052be15")))))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "E91A-D654" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "345d0308-199e-4270-a108-733d716a1c9c"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
