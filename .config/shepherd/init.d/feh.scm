(define feh
  (make <service>
    #:provides '(feh)
    #:docstring "Runs `feh'"
    #:respawn? #f
    #:one-shot? #t
    #:start (make-system-constructor "feh --bg-scale ~/Pictures/ferdinand-stohr-NFs6dRTBgaM-unsplash.jpg")))

(register-services feh)
