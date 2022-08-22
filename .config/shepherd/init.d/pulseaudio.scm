(define pulseaudio
  (make <service>
    #:provides '(pulseaudio)
    #:docstring "Runs `pulseaudio'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop (make-kill-destructor)))

(register-services pulseaudio)
(start pulseaudio)
