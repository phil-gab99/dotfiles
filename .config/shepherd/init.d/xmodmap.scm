(define xmodmap
  (service '(xmodmap)
    #:documentation "Runs `xmodmap'"
    #:respawn? #f
    #:one-shot? #t
    #:start (make-system-constructor "xmodmap ~/.xmodmaprc")))
