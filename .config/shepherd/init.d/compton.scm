(define compton
  (make <service>
    #:provides '(compton)
    #:docstring "Runs `compton'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("compton"))
    #:stop (make-kill-destructor)))

(register-services compton)
