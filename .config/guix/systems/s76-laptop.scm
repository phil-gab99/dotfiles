(define-module (systems s76-laptop)
  #:use-module (systems base)
  #:use-module (gnu)
  #:use-module (gnu packages file-systems))

(operating-system
 (inherit base-operating-system)

 (host-name "s76-laptop")

 (swap-devices
  (list
   (swap-space (target
                (uuid "5c2ecf42-19e0-46c0-ba33-51ced052be15")))))

 (file-systems
  (cons*
   (file-system (device "/dev/nvme0n1p1")
                (mount-point "/boot/efi")
                (type "vfat"))
   (file-system (device "/dev/nvme0n1p3")
                (mount-point "/")
                (type "ext4"))
   %base-file-systems)))
