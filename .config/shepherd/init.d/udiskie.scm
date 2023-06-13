(use-modules
 (shepherd support))

(define udiskie
  (service '(udiskie)
    #:documentation "Runs `udiskie'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("udiskie" "-t"))
    #:stop (make-kill-destructor)))

(register-services (list udiskie))
