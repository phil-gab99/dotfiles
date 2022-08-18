(define pulseaudio
  (make <service>
    #:provides '(pulseaudio)
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop (make-kill-destructor)))

(register-services pulseaudio)
(start pulseaudio)
