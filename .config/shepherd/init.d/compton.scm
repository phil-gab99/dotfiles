(define compton
  (make <service>
    #:provides '(compton)
    #:docstring "Runs `compton'"
    #:respawn? #f
    #:start (make-forkexec-constructor '("compton"))
    #:stop (make-kill-destructor)))

(register-services compton)
(start compton)
