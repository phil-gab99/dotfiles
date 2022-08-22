(define gpg-agent
  (make <service>
    #:provides '(gpg-agent)
    #:docstring "Runs `gpg-agent'"
    #:respawn? #t
    #:start (make-system-constructor "gpg-connect-agent /bye")
    #:stop (make-system-destructor "gpgconf --kill gpg-agent")))

(register-services gpg-agent)
(start gpg-agent)
