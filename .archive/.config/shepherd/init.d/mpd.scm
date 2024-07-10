(define mpd
  (service '(mpd)
    #:documentation "Runs `mpd'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd" "--no-daemon"))
    #:stop (make-kill-destructor)))
