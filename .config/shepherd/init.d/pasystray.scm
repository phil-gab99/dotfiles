(define pasystray
  (make <service>
    #:provides '(pasystray)
    #:docstring "Runs `pasystray'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("pasystray"))
    #:stop (make-kill-destructor)))

(register-services pasystray)
