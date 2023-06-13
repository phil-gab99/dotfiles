(use-modules
 (shepherd support))

(define feh
  (service '(feh)
    #:documentation "Runs `feh'"
    #:respawn? #f
    #:one-shot? #t
    #:start (make-system-constructor "feh --bg-scale ~/Pictures/wp-color-triangle.png")))

(register-services (list feh))
