(define xmodmap
  (make <service>
    #:provides '(xmodmap)
    #:docstring "Runs `xmodmap'"
    #:respawn? #f
    #:one-shot? #t
    #:start (make-system-constructor "xmodmap ~/.xmodmaprc")))

(register-services xmodmap)
