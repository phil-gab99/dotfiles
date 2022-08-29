(define mcron
  (make <service>
    #:provides '(mcron)
    #:docstring "Runs `mcron'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop (make-kill-destructor)))

(register-services mcron)
(start mcron)
