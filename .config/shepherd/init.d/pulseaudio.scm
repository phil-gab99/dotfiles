(use-modules
 (shepherd support))

(define pulseaudio
  (service '(pulseaudio)
    #:documentation "Runs `pulseaudio'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop (make-kill-destructor)))

(register-services (list pulseaudio))
(start-in-the-background '(pulseaudio))
