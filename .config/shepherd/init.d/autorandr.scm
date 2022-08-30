(define autorandr
  (make <service>
    #:provides '(autorandr)
    #:docstring "Runs `autorandr'"
    #:respawn? #f
    #:one-shot? #t
    #:start (make-system-constructor "autorandr --change --force")))

(register-services autorandr)
