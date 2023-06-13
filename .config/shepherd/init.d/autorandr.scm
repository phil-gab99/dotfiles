(use-modules
 (shepherd support))

(define autorandr
  (service '(autorandr)
    #:documentation "Runs `autorandr'"
    #:respawn? #f
    #:one-shot? #t
    #:start (make-system-constructor "autorandr --change --force")))

(register-services (list autorandr))
