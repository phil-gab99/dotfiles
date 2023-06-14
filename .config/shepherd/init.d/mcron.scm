(define mcron
  (service '(mcron)
    #:documentation "Runs `mcron'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop (make-kill-destructor)))
