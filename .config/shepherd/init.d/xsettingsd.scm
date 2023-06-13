(use-modules
 (shepherd support))

(define xsettingsd
  (service '(xsettingsd)
    #:documentation "Runs `xsettingsd'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("xsettingsd"))
    #:stop (make-kill-destructor)))

(register-services (list xsettingsd))
