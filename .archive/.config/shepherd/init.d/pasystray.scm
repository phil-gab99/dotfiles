(define pasystray
  (service '(pasystray)
    #:documentation "Runs `pasystray'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("pasystray"))
    #:stop (make-kill-destructor)))
