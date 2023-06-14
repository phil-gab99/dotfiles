(define pulseaudio
  (service '(pulseaudio)
    #:documentation "Runs `pulseaudio'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop (make-kill-destructor)))
