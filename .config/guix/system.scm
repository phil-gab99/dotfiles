;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
 (gnu)
 (srfi srfi-1)
 (gnu system nss)
 (gnu services pm)
 (gnu services cups)
 (gnu services desktop)
 (gnu services docker)
 (gnu services networking)
 (gnu services virtualization)
 (gnu packages wm)
 (gnu packages cups)
 (gnu packages vim)
 (gnu packages gtk)
 (gnu packages xorg)
 (gnu packages emacs)
 (gnu packages gnome)
 (gnu packages mtools)
 (gnu packages linux)
 (gnu packages audio)
 (gnu packages gnuzilla)
 (gnu packages pulseaudio)
 (gnu packages web-browsers)
 (gnu packages version-control)
 (gnu packages package-management)
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
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "emacs")
         (specification->package "emacs-exwm")
         (specification->package "emacs-desktop-environment")
         (specification->package "nss-certs")
         git
         ntfs-3g
         ;; exfat-utils
         fuse-exfat
         vim
         nix
         pulseaudio
         tlp
         ;; xf86-input-libinout
         gvfs)
   %base-packages))
 (services
  (append
   (list (service gnome-desktop-service-type)
         (service openssh-service-type)
         (service tor-service-type)
         (service cups-service-type
                  (cups-configuration
                   (web-interface? #t)
                   (extensions
                    (list cups-filters))))
         (service nix-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services))
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
