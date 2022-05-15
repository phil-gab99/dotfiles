(define gpg-agent
  (make <service>
    #:provides '(gpg-agent)
    #:respawn? #t
    #:start (make-system-constructor "gpg-connect-agent /bye")
    #:stop  (make-system-destructor "gpgconf --kill gpg-agent")))

(define mcron
  (make <service>
    #:provides '(mcron)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop  (make-kill-destructor)))

(define pulseaudio
  (make <service>
    #:provides '(pulseaudio)
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop  (make-kill-destructor)))

(define ssh-agent
  (make <service>
    #:provides '(ssh-agent)
    #:respawn? #t
    #:start (make-system-constructor "ssh-agent")
    #:stop  (make-system-destructor "ssh-agent -k")))

(register-services gpg-agent mcron pulseaudio ssh-agent)
(action 'shepherd 'daemonize)

;; Start user services
(for-each start '(gpg-agent mcron pulseaudio ssh-agent))
