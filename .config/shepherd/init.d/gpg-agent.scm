(use-modules
 (shepherd support))

(define gpg-agent
  (service '(gpg-agent)
    #:documentation "Runs `gpg-agent'"
    #:respawn? #t
    #:start (make-system-constructor "gpg-connect-agent /bye")
    #:stop (make-system-destructor "gpgconf --kill gpg-agent")))

(register-services (list gpg-agent))
(start-in-the-background '(gpg-agent))
