(define mpd
  (service '(mpd)
    #:documentation "Runs `mpd'"
    #:respawn? #t
    #:start (make-system-constructor "mpd")
    #:stop (make-system-destructor "mpd --kill")))
