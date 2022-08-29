(define udiskie
  (make <service>
    #:provides '(udiskie)
    #:docstring "Runs `udiskie'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("udiskie" "-t"))
    #:stop (make-kill-destructor)))

(register-services udiskie)
