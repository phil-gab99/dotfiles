(use-modules
 (shepherd support))

(define pasystray
  (service '(pasystray)
    #:documentation "Runs `pasystray'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("pasystray"))
    #:stop (make-kill-destructor)))

(register-services (list pasystray))
