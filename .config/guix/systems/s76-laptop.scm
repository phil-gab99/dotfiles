(define-module (systems s76-laptop)
  #:use-module (systems base)
  #:use-module (gnu))

(use-package-modules file-systems)

(operating-system
 (inherit base-operating-system)

 (host-name "s76-laptop")

 (swap-devices
  (list
   (swap-space (target
                (uuid "007cbe9f-5d70-4ded-bd10-898993e4de74")))))

 (file-systems
  (cons*
   (file-system (device "/dev/nvme0n1p1")
                (mount-point "/boot/efi")
                (type "vfat"))
   (file-system (device "/dev/nvme0n1p2")
                (mount-point "/")
                (type "ext4"))
   (file-system (device "/dev/nvme0n1p4")
                (mount-point "/home")
                (type "ext4"))
   %base-file-systems)))
