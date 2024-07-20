(define-module (pg systems s76-laptop)
  #:use-module (pg systems base)
  #:use-module (pg home services bash)
  #:use-module (pg home services emacs)
  #:use-module (pg home services emulators)
  #:use-module (pg home services media)
  #:use-module (pg home services syncthing)
  #:use-module (pg home services wm)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services mcron)
  #:use-module (guix gexp))

(use-package-modules file-systems fonts gnome gnome-xyz gnupg music terminals
                     video wm xdisorg)
(use-service-modules base)
(use-system-modules accounts)

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

(define %user
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
      					"charge"
      					"libvirt"
      					"docker"
      					"realtime"
      					"lp"        ;; control bluetooth devices
      					"audio"     ;; control audio devices
      					"video")))) ;; control video devices

(define %user-dotfiles
  (string-append (user-account-home-directory %user) "/.dotfiles"))

(define %pass-sync-job
  #~(job '(next-hour (range 0 24 4))
         #$(program-file
            "pass-sync.scm"
            #~(begin
                (use-modules (guix build utils))
                (setenv "PATH"
                        (string-append (getenv "PATH")
                                       ":"
                                       #$(user-account-home-directory %user)
                                       "/.guix-home/profile/bin:"
                                       #$(user-account-home-directory %user)
                                       "/bin"))
                (invoke "sync-passwords")))))

(define %home
  (home-environment
   (services (list (service home-bash-service-type
      		            (home-bash-configuration
      		             (bash-profile
                              (string-append %user-dotfiles
                                             "/.templates/bash_profile"))
      		             (bashrc
                              (string-append %user-dotfiles
                                             "/.templates/bashrc"))))
      	           (service home-dbus-service-type)
      	           (service home-dotfiles-service-type
      		            (home-dotfiles-configuration
      		             (source-directory %user-dotfiles)
      		             (directories '(".files"))))
      	           (service home-emacs-service-type)
                   (service home-emulators-service-type)
      	           (service home-gpg-agent-service-type
      		            (home-gpg-agent-configuration
      		             (pinentry-program
      		              (file-append pinentry-emacs "/bin/pinentry-emacs"))
      		             (ssh-support? #t)
      		             (default-cache-ttl 28800)
      		             (max-cache-ttl 28800)
      		             (default-cache-ttl-ssh 28800)
      		             (max-cache-ttl-ssh 28800)))
      	           (service home-mcron-service-type
      		            (home-mcron-configuration
      		             (jobs (list %pass-sync-job))))
      	           (service home-media-service-type)
      	           (service home-syncthing-service-type
                            (home-syncthing-configuration
                             (user (user-account-name %user))
                             (home (user-account-home-directory %user))))
      	           (service home-wm-service-type)))))

(define %system
  (operating-system
    (host-name "s76-laptop")
    (keyboard-layout (keyboard-layout "us")) 

    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
      	         (bootloader grub-efi-removable-bootloader)
      	         (targets '("/boot/efi"))
      	         (keyboard-layout keyboard-layout)))

    (swap-devices
     (list
      (swap-space (target (uuid "007cbe9f-5d70-4ded-bd10-898993e4de74")))))

    (file-systems
     (cons* (file-system (device "/dev/nvme0n1p1")
      	                 (mount-point "/boot/efi")
      	                 (type "vfat"))
            (file-system (device "/dev/nvme0n1p2")
      	                 (mount-point "/")
      	                 (type "ext4"))
            (file-system (device "/dev/nvme0n1p4")
      	                 (mount-point "/home")
      	                 (type "ext4"))
            (file-system (device "/dev/nvme1n1p1")
                         (mount-point "/d/d1")
                         (type "ext4"))
            %base-file-systems))

    (users
     (cons %user
           %base-user-accounts))

    (groups
     (list (user-group
            (system? #t)
            (name "charge"))))

    (services (list (udev-rules-service 'charge-thresholds
      				        %charge-thresholds-udev-rule)))))

(if (getenv "RUNNING_GUIX_HOME")
    %home
    (system-config #:home %home
                   #:system %system))
