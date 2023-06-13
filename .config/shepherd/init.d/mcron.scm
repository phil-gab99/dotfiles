(use-modules
 (shepherd support))

(define mcron
  (service '(mcron)
    #:documentation "Runs `mcron'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop (make-kill-destructor)))

(register-services (list mcron))
(start-in-the-background '(mcron))
