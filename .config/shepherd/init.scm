(define gpg-agent
  (make <service>
    #:provides '(gpg-agent)
    #:respawn? #t
    #:start (make-system-constructor "gpg-connect-agent /bye")
    #:stop (make-system-destructor "gpgconf --kill gpg-agent")))

(define pulseaudio
  (make <service>
    #:provides '(pulseaudio)
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop  (make-kill-destructor)))

(register-services gpg-agent pulseaudio)
(action 'shepherd 'daemonize)

;; Start user services
(for-each start '(gpg-agent pulseaudio))
