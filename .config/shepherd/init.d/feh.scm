(define feh
  (make <service>
    #:provides '(feh)
    #:docstring "Runs `feh'"
    #:respawn? #f
    #:one-shot? #t
    #:start (make-system-constructor "feh --bg-scale ~/Pictures/wp-color-triangle.png")))

(register-services feh)
