(define compton
  (service '(compton)
    #:documentation "Runs `compton'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("compton"))
    #:stop (make-kill-destructor)))
